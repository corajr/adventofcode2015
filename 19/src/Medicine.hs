module Medicine ( module Medicine.Parser
                , module Medicine
                ) where

import qualified Data.Set as Set
import Control.Monad (guard)
import Medicine.Parser

getDistinctMolecules :: Medicine -> Int
getDistinctMolecules = Set.size . getMolecules

getMolecules :: Medicine -> Set.Set Molecule
getMolecules (Medicine subst m) = Set.fromList $ do
  Substitute from to <- subst
  (atom, i) <- zip m [0..]
  guard (from == atom)
  let (xs, _:ys) = splitAt i m
  return $ xs ++ to ++ ys

buildInSteps :: Medicine -> Int
buildInSteps med = go 0 (Set.singleton ["e"])
  where go i xs
          | molecule med `Set.member` xs = i
          | otherwise = go (i + 1) (Set.unions (map update (Set.toList xs)))
        update x = getMolecules $ med { molecule = x }
