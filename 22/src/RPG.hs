{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module RPG ( module RPG
           , module RPG.Spell) where

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad (when)
import Control.Arrow ((&&&))
import RPG.Spell

data Player = Player
     deriving (Show, Eq)

data Enemy = Enemy
     deriving (Show, Eq)

data Side = P Player | E Enemy
          deriving (Show, Eq)

class Health a where
  getHP :: a -> Int
  modHP :: Int -> a -> a

data PlayerStats =
  PlayerStats { hp :: Int
              , mana :: Int
              , armor :: Int
              } deriving (Show, Eq)

data BossStats =
  BossStats { hp' :: Int
            , damage :: Int
            } deriving (Show, Eq)

data Stats a = PS PlayerStats | BS BossStats
             deriving (Show, Eq)

instance Health (Stats a) where
  getHP (PS ps) = hp ps
  getHP (BS bs) = hp' bs

  modHP i (PS ps) = PS $ ps { hp = hp ps + i }
  modHP i (BS bs) = BS $ bs { hp' = hp' bs + i }

data Game = Game { playerStats :: Stats Player
                 , enemyStats :: Stats Enemy
                 , activeEffects :: Effects
                 , spellList :: [Spell]
                 , turn :: Side
                 } deriving (Show, Eq)

type GameState = StateT Game (Except String)

mtPlayer = PlayerStats 0 0 0
mtBoss = BossStats 0 0

mtGame = Game { playerStats = PS mtPlayer
              , enemyStats = BS mtBoss
              , activeEffects = Map.empty
              , spellList = []
              , turn = P Player
              }

takeTurn :: GameState ()
takeTurn = do
  t <- gets turn
  case t of
    P Player -> cast >> switchTurn
    E Enemy -> bossAttack >> switchTurn

cast :: GameState ()
cast = do
  xs <- gets spellList
  when (null xs) $
    lift $ throwE "called cast without any spells"
  let (x:spells) = xs
  deduct x
  case x of
    Now s -> castImmediate s
    Later s -> castEffect s
  modify (\game -> game { spellList = spells })

deduct :: Spell -> GameState ()
deduct s = do
  (PS p) <- gets playerStats
  let cost' = cost s
      p' = p { mana = mana p - cost' }
  modify (\game -> game { playerStats = PS p' })

applyStats :: SpellStats -> Game -> Game
applyStats stats game =
  let (PS p') = modHP (spPlayerHP stats) (playerStats game)
      armUp = spPlayerArmor stats
      p'' = if armUp > 0 then p' { armor = armUp } else p'
      p''' = p'' { mana = mana p'' + spPlayerMana stats }
      e' = modHP (spEnemyHP stats) (enemyStats game)
  in game { playerStats = PS p''', enemyStats = e' }

castImmediate :: SpellType () -> GameState ()
castImmediate s = modify (applyStats (getStats s))

castEffect :: SpellType Int -> GameState ()
castEffect s = do
  actives <- gets activeEffects
  when (s `Map.member` actives) $
    lift $ throwE ("Tried to cast " ++ show s ++ " while already active")
  let actives' = Map.insert s (getEffect s) actives
  modify (\game -> game { activeEffects = actives' })
  when (s == Shield) $
    applyEffect (getEffect s)

bossAttack :: GameState ()
bossAttack = do
  (BS enemy) <- gets enemyStats
  play@(PS p) <- gets playerStats
  let amt = damage enemy - armor p
      p' = modHP (-amt) play
  modify (\game -> game { playerStats = p'})

switchTurn :: GameState ()
switchTurn = do
  t <- gets turn
  let t' = case t of
             P Player -> E Enemy
             E Enemy -> P Player
  modify (\game -> game { turn = t'})

updateEffect :: Effect -> Maybe Effect
updateEffect effect
  | duration' == 0 = Nothing
  | otherwise = Just $ effect { eDuration = duration' }
  where duration' = eDuration effect - 1

resetArmor :: GameState ()
resetArmor = do
  (PS p) <- gets playerStats
  modify (\game -> game { playerStats = PS p {armor = 0}})

applyEffect :: Effect -> GameState ()
applyEffect e = do
  modify (applyStats (eStats e))
  modify (\game -> game { activeEffects = Map.update updateEffect (eSpell e) (activeEffects game) })

applyEffects :: GameState ()
applyEffects = do
  actives <- gets activeEffects
  resetArmor
  mapM_ applyEffect (Map.elems actives)

winner :: Game -> Maybe Side
winner Game { playerStats = p, enemyStats = e }
  | getHP e <= 0 = Just (P Player)
  | getHP p <= 0 = Just (E Enemy)
  | otherwise = Nothing

oneRound :: GameState ()
oneRound = applyEffects >> takeTurn

battle :: GameState Side
battle = do
  oneRound
  game <- get
  case winner game of
    Just side -> return side
    Nothing -> battle

execGame :: GameState a -> Game -> Either String Game
execGame m = runExcept . execStateT m

evalGame :: GameState a -> Game -> Either String a
evalGame m = runExcept . evalStateT m

playerWins :: Stats Enemy -> [Spell] -> Bool
playerWins enemy spells = Right (P Player) == evalGame battle game
  where game = mtGame { playerStats = playerStart
                      , enemyStats = enemy
                      , spellList = spells }

playerStart :: Stats Player
playerStart = PS $ PlayerStats 50 500 0

cheapestWin :: Stats Enemy -> Int
cheapestWin enemy =
  minimum . map snd . filter fst . map (playerWins enemy &&& (sum . map cost)) $ spellSequences

spellSequences = [[Now MagicMissile]]
