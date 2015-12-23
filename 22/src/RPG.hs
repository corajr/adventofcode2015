{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module RPG ( module RPG
           , module RPG.Spell) where

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.Random
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.String.Interpolate
import Control.Monad (when, replicateM)
import Data.Maybe (isJust)
import Data.List ((\\))
import Data.Either (rights)
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
                 , spellsCast :: Int
                 , manaSpent :: Int
                 , turn :: Side
                 , strategy :: Strategy
                 , winner :: Maybe Side
                 } deriving (Show, Eq)

data Strategy = ListStrat [Spell]
              | RandStrat
              deriving (Show, Eq)

type GameState a = ExceptT String (RWST () String Game (Rand StdGen)) a

mtPlayer :: PlayerStats
mtPlayer = PlayerStats 0 0 0

mtBoss :: BossStats
mtBoss = BossStats 0 0

mtGame :: Game
mtGame = Game { playerStats = PS mtPlayer
              , enemyStats = BS mtBoss
              , activeEffects = Map.empty
              , spellsCast = 0
              , manaSpent = 0
              , turn = P Player
              , winner = Nothing
              , strategy = RandStrat
              }

takeTurn :: GameState ()
takeTurn = do
  t <- lift $ gets turn
  case t of
    P Player -> cast >> switchTurn
    E Enemy -> bossAttack >> switchTurn

cast :: GameState ()
cast = do
  game <- lift get
  x <- case strategy game of
         ListStrat xs -> listStrat xs
         RandStrat -> randStrat
  deduct x
  case x of
    Now s -> castImmediate s
    Later s -> castEffect s
  lift $ modify (\game -> game { spellsCast = spellsCast game + 1
                               , manaSpent = manaSpent game + cost x})

deduct :: Spell -> GameState ()
deduct s = do
  (PS p) <- lift $ gets playerStats
  let cost' = cost s
      p' = p { mana = mana p - cost' }
  when (mana p' < 0) $
    throwE ("Tried to cast " ++ show s ++ " while no mana left")
  lift $ modify (\game -> game { playerStats = PS p' })

applyStats :: SpellStats -> Game -> Game
applyStats stats game =
  let (PS p') = modHP (spPlayerHP stats) (playerStats game)
      armUp = spPlayerArmor stats
      p'' = if armUp > 0 then p' { armor = armUp } else p'
      p''' = p'' { mana = mana p'' + spPlayerMana stats }
      e' = modHP (spEnemyHP stats) (enemyStats game)
  in game { playerStats = PS p''', enemyStats = e' }

castImmediate :: SpellType () -> GameState ()
castImmediate s = lift $ modify (applyStats (getStats s))

castEffect :: SpellType Int -> GameState ()
castEffect s = do
  actives <- lift $ gets activeEffects
  when (s `Map.member` actives) $
    throwE ("Tried to cast " ++ show s ++ " while already active")
  let actives' = Map.insert s (getEffect s) actives
  lift $ modify (\game -> game { activeEffects = actives' })
  lift $ tell [i|Player casts #{s}\n|]
  when (s == Shield) $
    applyEffect (getEffect s)

bossAttack :: GameState ()
bossAttack = do
  (BS enemy) <- lift $ gets enemyStats
  play@(PS p) <- lift $ gets playerStats
  let amt = damage enemy - armor p
      p' = modHP (-amt) play
  lift $ tell [i|Boss attacks for #{amt} damage!\n|]
  lift $ modify (\game -> game { playerStats = p'})

switchTurn :: GameState ()
switchTurn = do
  t <- lift $ gets turn
  let t' = case t of
             P Player -> E Enemy
             E Enemy -> P Player
  lift $ modify (\game -> game { turn = t'})

updateEffect :: Effect -> Maybe Effect
updateEffect effect
  | duration' == 0 = Nothing
  | otherwise = Just $ effect { eDuration = duration' }
  where duration' = eDuration effect - 1

resetArmor :: GameState ()
resetArmor = do
  (PS p) <- lift $ gets playerStats
  lift $ modify (\game -> game { playerStats = PS p {armor = 0}})

applyEffect :: Effect -> GameState ()
applyEffect e = do
  lift $ modify (applyStats (eStats e))
  lift $ modify (\game -> game { activeEffects = Map.update updateEffect (eSpell e) (activeEffects game) })
  e' <- lift $ gets (Map.lookup (eSpell e) . activeEffects)
  case e' of
    Just e'' -> lift $ tell [i|#{eSpell e''}; its timer is now #{eDuration e''}.\n|]
    Nothing -> lift $ tell [i|#{eSpell e} wears off.\n|]

applyEffects :: GameState ()
applyEffects = do
  actives <- lift $ gets activeEffects
  resetArmor
  mapM_ applyEffect (Map.elems actives)
  checkIfWon

checkWinner :: Game -> Maybe Side
checkWinner Game { playerStats = p, enemyStats = e }
  | getHP e <= 0 = Just (P Player)
  | getHP p <= 0 = Just (E Enemy)
  | otherwise = Nothing

checkIfWon :: GameState ()
checkIfWon = do
  w <- lift $ gets checkWinner
  when (isJust w) $
    lift $ tell [i|#{w} wins.\n|]
  lift $ modify (\game -> game { winner = w } )

oneRound :: GameState ()
oneRound = applyEffects >> takeTurn

battle :: GameState (Side, Int)
battle = do
  tellTurn
  oneRound
  won <- lift $ gets winner
  case won of
    Just side -> do
      mana' <- lift $ gets manaSpent
      return (side, mana')
    Nothing -> battle

tellTurn :: GameState ()
tellTurn = do
  t <- lift $ gets turn
  (PS p) <- lift $ gets playerStats
  (BS e) <- lift $ gets enemyStats
  case t of
    P Player -> lift $ tell "-- Player turn --\n"
    E Enemy -> lift $ tell "-- Boss turn --\n"
  lift $ tell [i|- Player has #{hp p} hit points, #{armor p} armor, #{mana p} mana\n|]
  lift $ tell [i|- Boss has #{hp' e} hit points\n|]

runGame :: GameState a -> Game -> StdGen -> (Either String a, Game, String)
runGame m g gen = (`evalRand` gen) . (\m' -> runRWST m' () g) $ runExceptT m

evalGame :: GameState a -> Game -> StdGen -> Either String a
evalGame m g = (\(a,_,_) -> a) . runGame m g

execGame :: GameState a -> Game -> StdGen -> Either String Game
execGame m g = f . runGame m g
  where f (Right _, s', _) = Right s'
        f (Left err, _, _ ) = Left err

showGame :: GameState a -> Game -> StdGen -> Either String String
showGame m g = f . runGame m g
  where f (Right _, _, w) = Right w
        f (Left err, _, _ ) = Left err

playGame :: Stats Enemy -> [Spell] -> Either String Side
playGame e spells' = fst <$> evalGame battle game (mkStdGen 0)
  where game = mkGame e (Just spells')

randStrat :: GameState Spell
randStrat = do
  actives <- lift $ gets activeEffects
  (PS p) <- lift $ gets playerStats
  let manaRemaining = mana p
      spells' = spells \\ map Later (Map.keys actives)
      spells'' = filter ((< manaRemaining) . cost) spells'
  when (null spells'') $
    throwE "No spells available"
  lift $ uniform spells''

listStrat :: [Spell] -> GameState Spell
listStrat spells' = do
  i <- lift $ gets spellsCast
  return $ spells' !! i

mkGame :: Stats Enemy -> Maybe [Spell] -> Game
mkGame enemy spells' =
  mtGame { playerStats = playerStart
         , enemyStats = enemy
         , strategy = strat
         }
  where strat = case spells' of
                  Just xs -> ListStrat xs
                  Nothing -> RandStrat

playerStart :: Stats Player
playerStart = PS $ mtPlayer { hp = 50, mana = 500 }

execGame' :: GameState a -> Game -> Either String Game
execGame' m g = execGame m g (mkStdGen 0)

showGame' :: GameState a -> Game -> Either String String
showGame' m g = showGame m g (mkStdGen 0)

playerWins :: Stats Enemy -> [Spell] -> Bool
playerWins enemy spells' =
  case result of
    Right (P Player, _) -> True
    _ -> False
  where game = mkGame enemy (Just spells')
        result = evalGame battle game (mkStdGen 0)

randGame :: Stats Enemy -> StdGen -> Either String (Side, Int)
randGame enemy = evalGame battle game
  where game = mkGame enemy Nothing

cheapestWin :: Stats Enemy -> IO Int
cheapestWin enemy = fmap (minimum . map snd . filter ((==) (P Player) . fst) . rights) games
  where games :: IO [Either String (Side, Int)]
        games = replicateM 100000 oneGame
        oneGame = do
          gen <- newStdGen
          return (randGame enemy gen)
  -- fmap (map (runGame battle . mkGame enemy &&& (sum . map cost))) spellSequences
  -- fmap (minimum . map snd . filter fst . map (playerWins enemy &&& (sum . map cost))) spellSequences
