module Main where

import Flight

main = do
  input <- readFile "input.txt"
  let constraints = map parseConstraint (lines input)
  print $ maxDistance 2503 constraints
