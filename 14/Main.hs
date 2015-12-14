module Main where

import Flight

main = do
  input <- readFile "input.txt"
  let constraints = map parseConstraint (lines input)
  print $ maxScore 2503 constraints
