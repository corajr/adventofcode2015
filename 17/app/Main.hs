module Main where

import Combine

main :: IO ()
main = do
  input <- readFile "input.txt"
  let capacities = map read (lines input)
  print $ combinationsMinimum capacities 150
