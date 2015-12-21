module RPG ( module RPG
           , module RPG.Shop) where

import Control.Monad.Trans.State
import Control.Arrow (first)
import RPG.Shop

data Stats = Stats { hp :: Int
                   , damage :: Int
                   , armor :: Int
                   } deriving (Show, Eq)

data Side = Player | Enemy
          deriving (Show, Eq)

data Game = Game { playerStats :: Stats
                 , enemyStats :: Stats
                 , turn :: Side
                 } deriving (Show, Eq)

type GameState = State Game

fight :: GameState ()
fight = do
  game <- get
  let (attacking, defending) = case turn game of
        Player -> (playerStats game, enemyStats game)
        Enemy -> (enemyStats game, playerStats game)
      attack = damage attacking - armor defending
      attack' = maximum [attack, 1]
      (attacking', defending') = (attacking, defending { hp = hp defending - attack' })
  case turn game of
    Player -> put (Game attacking' defending' Enemy)
    Enemy -> put (Game defending' attacking' Player)

winner :: Game -> Maybe Side
winner (Game p e _)
  | hp e <= 0 = Just Player
  | hp p <= 0 = Just Enemy
  | otherwise = Nothing

battle :: GameState Side
battle = do
  fight
  game <- get
  case winner game of
    Just side -> return side
    Nothing -> battle

playerWins :: Stats -> Stats -> Bool
playerWins player enemy = Player == evalState battle game
  where game = Game player enemy Player

equipPlayer :: Equipment -> Stats
equipPlayer (Equipment (Weapon weapon') maybeArmor maybeRing1 maybeRing2) =
  let stats = Stats { hp = 100, damage = 0, armor = 0}
      stats' = stats { damage = damage stats + itemDamage weapon' }
      stats'' = case maybeArmor of
                  Just (Armor armor') -> stats' { armor = armor stats' + itemArmor armor' }
                  Nothing -> stats'
      stats''' = addRing maybeRing1 stats''
      stats'''' = addRing maybeRing2 stats'''
  in stats''''
  where addRing (Just (Ring x)) s = s { damage = damage s + itemDamage x, armor = armor s + itemArmor x }
        addRing Nothing s = s

cheapestWin :: Stats -> Int
cheapestWin enemy =
  minimum . map snd . filter (\(p, _) -> playerWins p enemy) $ map (first equipPlayer) allEquipment
