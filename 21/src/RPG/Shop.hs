module RPG.Shop where

import Control.Monad (guard, when)
import Control.Arrow ((&&&))
import Data.Maybe (isJust)

class IsItem a where
  item :: a -> Item

data Item =
  Item { name :: String
       , itemCost :: Int
       , itemDamage :: Int
       , itemArmor :: Int
       } deriving (Show, Eq)

newtype Weapon = Weapon Item
               deriving (Show, Eq)

instance IsItem Weapon where
  item (Weapon i) = i

newtype Armor = Armor Item
              deriving (Show, Eq)

instance IsItem Armor where
  item (Armor i) = i

newtype Ring = Ring Item
             deriving (Show, Eq)

instance IsItem Ring where
  item (Ring i) = i

data Equipment =
 Equipment { weapon :: Weapon
           , equipArmor :: Maybe Armor
           , equipRing1 :: Maybe Ring
           , equipRing2 :: Maybe Ring
           } deriving (Show, Eq)

weapons :: [Weapon]
weapons =
  [ Weapon (Item "Dagger" 8 4 0)
  , Weapon (Item "Shortsword" 10 5 0)
  , Weapon (Item "Warhammer" 25 6 0)
  , Weapon (Item "Longsword" 40 7 0)
  , Weapon (Item "Greataxe" 74 8 0)
  ]

armors :: [Armor]
armors =
  [ Armor (Item "Leather" 13 0 1)
  , Armor (Item "Chainmail" 31 0 2)
  , Armor (Item "Splintmail" 53 0 3)
  , Armor (Item "Bandedmail" 75 0 4)
  , Armor (Item "Platemail" 102 0 5)
  ]

rings :: [Ring]
rings =
  [ Ring (Item "Damage +1" 25 1 0)
  , Ring (Item "Damage +2" 50 2 0)
  , Ring (Item "Damage +3" 100 3 0)
  , Ring (Item "Defense +1" 20 0 1)
  , Ring (Item "Defense +2" 40 0 2)
  , Ring (Item "Defense +3" 80 0 3)
  ]

type Arrangement = (Int, Maybe Int, Maybe Int, Maybe Int)

arrangements :: [Arrangement]
arrangements = do
  w <- [0..length weapons - 1]
  a <- Nothing : map Just [0..length armors - 1]
  r1 <- Nothing : map Just [0..length rings - 1]
  r2 <- Nothing : map Just [0..length rings - 1]
  when (isJust r1 && isJust r2) $
    guard (r1 /= r2)
  return (w, a, r1, r2)

arrange :: Arrangement -> Equipment
arrange (w, a, r1, r2) =
  let weap = weapons !! w
      armor' = fmap (armors !!) a
      ring1 = fmap (rings !!) r1
      ring2 = fmap (rings !!) r2
  in Equipment weap armor' ring1 ring2

cost :: Equipment -> Int
cost (Equipment w a r1 r2) = sum [ itemCost (item w)
                                 , cost' a
                                 , cost' r1
                                 , cost' r2
                                 ]
  where cost' (Just x) = itemCost $ item x
        cost' Nothing = 0

allEquipment :: [(Equipment, Int)]
allEquipment = map ((id &&& cost) . arrange) arrangements
