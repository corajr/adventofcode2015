module Main where

import Distances

partOne = do
  input <- readFile "input.txt"
  let graph = parse input
  print $ travelingSalesman graph

main = partOne
