module Gifts where

import Data.List (findIndex)
import Math.NumberTheory.Primes.Factorisation (sigma)

houses = [1..]

gifts = map ((*10) . sigma 1) houses

findHouse p = fmap (+1) $ findIndex p gifts
