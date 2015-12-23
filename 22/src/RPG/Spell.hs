{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module RPG.Spell where

import qualified Data.Map.Strict as Map

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
  Recharge -> 5

getEffect :: SpellType Int  -> Effect
getEffect x = Effect x (getStats x) (getDuration x)

cost :: Spell -> Int
cost x =
  case x of
    Now s -> f s
    Later s -> f s
  where f = spCost . getStats

spells = [ Now MagicMissile
         , Now Drain
         , Later Shield
         , Later Poison
         , Later Recharge]

validSequence :: [Spell] -> Bool
validSequence spells' = notInRange (Later Shield) 2 && notInRange (Later Poison) 2 && notInRange (Later Recharge) 2
  where notInRange x n = and [all (\(a,b) -> a /= b || (a == b && a /= x)) (zip (shifted i) spells') | i <- [1..n]]
        shifted n = drop n spells'

-- getSequenceOf :: Int -> IO [Spell]
-- getSequenceOf i = do
--   xs <- runRVar (choices i spells) StdRandom
--   if validSequence xs
--     then return xs
--     else getSequenceOf i

-- spellSequences :: IO [[Spell]]
-- spellSequences = return [[ Later Shield
--                          , Later Recharge
--                          , Later Poison
--                          , Later Shield
--                          , Later Recharge
--                          , Later Poison
--                          , Later Shield
--                          , Later Recharge
--                          , Later Poison
--                          , Later Shield
--                          , Later Recharge
--                          , Later Poison
--                          , Now MagicMissile
--                          , Now MagicMissile
--                          ]]

-- spellSequences :: IO [[Spell]]
-- spellSequences =
--   fmap concat . forM [13..13] $ \i ->
--     replicateM 1000 (getSequenceOf i)
