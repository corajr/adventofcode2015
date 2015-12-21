module Gifts where

import Data.List (findIndex)
import qualified Data.Set as Set
import Math.NumberTheory.Primes.Factorisation (divisors, divisorSum)

houses = [1..]

partOne = divisorSum
partTwo n = sum . filter (\x -> x*50 >= n) . Set.toList $ divisors n

gifts = map ((*11) . partTwo) houses

findHouse p = fmap (+1) $ findIndex p gifts
