{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module RPG.Spell where

import qualified Data.Map.Strict as Map
import Control.Arrow ((&&&))

data SpellType a where
  MagicMissile :: SpellType ()
  Drain :: SpellType ()
  Shield :: SpellType Int
  Poison :: SpellType Int
  Recharge :: SpellType Int

deriving instance Show (SpellType a)
deriving instance Eq (SpellType a)
deriving instance Ord (SpellType a)

data SpellStats =
  SpellStats { spCost :: Int
             , spEnemyHP :: Int
             , spPlayerHP :: Int
             , spPlayerMana :: Int
             , spPlayerArmor :: Int
             } deriving (Show, Eq)

data Spell = Now (SpellType ())
           | Later (SpellType Int)
           deriving (Show, Eq)

data Effect =
  Effect { eSpell :: SpellType Int
         , eStats :: SpellStats
         , eDuration :: Int
         } deriving (Show, Eq)

type Effects = Map.Map (SpellType Int) Effect

mt :: SpellStats
mt = SpellStats 0 0 0 0 0

getStats :: SpellType a -> SpellStats
getStats x = case x of
  MagicMissile -> mt { spCost = 53, spEnemyHP = -4 }
  Drain -> mt { spCost = 73, spEnemyHP = -2, spPlayerHP = 2 }
  Shield -> mt { spCost = 113, spPlayerArmor = 7 }
  Poison -> mt { spCost = 173, spEnemyHP = -3 }
  Recharge -> mt { spCost = 229, spPlayerMana = 101 }

getDuration :: SpellType Int -> Int
getDuration x = case x of
  Shield -> 6
  Poison -> 6
  Recharge -> 6

getEffect :: SpellType Int  -> Effect
getEffect x = Effect x (getStats x) (getDuration x)

cost :: Spell -> Int
cost x =
  case x of
    Now s -> f s
    Later s -> f s
  where f = spCost . getStats
