module Combine where

import Data.List (subsequences)

combinations :: [Int] -> Int -> Int
combinations capacities amount =
  length [xs | xs <- subsequences capacities, sum xs == amount]

combinationsMinimum :: [Int] -> Int -> Int
combinationsMinimum capacities amount =
  let combos = [xs | xs <- subsequences capacities, sum xs == amount]
      least = minimum (map length combos)
  in length $ [ xs | xs <- combos, length xs == least]
