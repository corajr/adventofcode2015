module Combine where

import Data.List (subsequences)

combinations :: [Int] -> Int -> Int
combinations capacities amount =
  length [xs | xs <- subsequences capacities, sum xs == amount]
