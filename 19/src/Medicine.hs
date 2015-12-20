module Medicine ( module Medicine.Parser
                , module Medicine
                ) where

import qualified Data.Set as Set
import Control.Monad (guard)
import Medicine.Parser

getDistinctMolecules :: Medicine -> Int
getDistinctMolecules = Set.size . Set.fromList . getMolecules

getMolecules :: Medicine -> [Molecule]
getMolecules (Medicine subst m) = do
  Substitute from to <- subst
  (atom, i) <- zip m [0..]
  guard (from == atom)
  let (xs, _:ys) = splitAt i m
  return $ xs ++ to ++ ys
