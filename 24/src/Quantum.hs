module Quantum where

import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.List (sortBy, subsequences)
import Data.Ord (comparing)
import Control.Monad (guard)

type Package = Int

type Arrangement = [Package]

-- subsets :: Ord a => Set a -> [Set a]
-- subsets set
--   | Set.null set = []
--   | otherwise = let (x, xs) = Set.deleteFindMin set
--                     f ys r = ys : Set.insert x ys : r
--                 in Set.singleton x : foldr f [] (subsets xs)

sum' :: Set Int -> Int
sum' = sum . Set.toList

arrangements :: [Package] -> [Arrangement]
arrangements packages = ar
  where n = length packages
        ar = sortBy (comparing product) . filter ((< n `div` 4) . length) $ subsequences packages

bestEntanglement :: [Package] -> Int
bestEntanglement packages = head . map product . filter valid $ arrangements packages
  where valid arr = let others = packSet \\ Set.fromList arr
                    in sum' others == 3 * sum arr
        packSet = Set.fromList packages
