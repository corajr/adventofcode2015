module Medicine ( module Medicine.Parser
                , module Medicine
                ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Control.Parallel.Strategies
import Control.Monad (guard)
import Data.Maybe (isJust)
import Medicine.Parser

getDistinctMolecules :: Medicine -> Int
getDistinctMolecules = Set.size . getMolecules

getMolecules :: Medicine -> Set.HashSet Molecule
getMolecules med@(Medicine _ m) = Set.fromList $ do
  (atom, i) <- zip m [0..]
  to <- Map.lookupDefault [] atom (toSubMap med)
  let (xs, _:ys) = splitAt i m
  return $ xs ++ to ++ ys

getPriorGen :: Medicine -> Set.HashSet Molecule
getPriorGen med@(Medicine _ m) = Set.fromList $ do
  let revMap = toRevMap med
      longest = maximum . map length $ Map.keys revMap
      n = length m
  i <- [0..n-1]
  j <- [1..longest]
  let substr = take j (drop i m)
      atom = Map.lookup substr revMap
  case atom of
    Just "e" -> if length substr == n
                   then return ["e"]
                   else []
    Just a -> return $ take i m ++ [a] ++ drop (i+j) m
    Nothing -> []

buildInSteps :: Medicine -> Int
buildInSteps med = go 0 (Set.singleton $ molecule med)
  where go i xs
          | ["e"] `Set.member` xs = i
          | otherwise = go (i + 1) (Set.unions (parMap rdeepseq update (Set.toList xs)))
        update x = getPriorGen $ med { molecule = x }
