module Main where

import Seating

main = do
  input <- readFile "input.txt"
  let values = parseValues input
  print $ maxValue values
