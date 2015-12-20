module Medicine ( module Medicine.Parser
                , module Medicine
                ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Control.Parallel.Strategies
import Control.Monad (guard)
import Medicine.Parser

getDistinctMolecules :: Medicine -> Int
getDistinctMolecules = Set.size . getMolecules

getMolecules :: Medicine -> Set.HashSet Molecule
getMolecules med@(Medicine _ m) = Set.fromList $ do
  (atom, i) <- zip m [0..]
  to <- Map.lookupDefault [] atom (toSubMap med)
  let (xs, _:ys) = splitAt i m
  return $ xs ++ to ++ ys

buildInSteps :: Medicine -> Int
buildInSteps med = go 0 (Set.singleton ["e"])
  where go i xs
          | molecule med `Set.member` xs = i
          | otherwise = go (i + 1) (Set.unions (parMap rdeepseq update (Set.toList xs)))
        update x = getMolecules $ med { molecule = x }
