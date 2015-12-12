module Main where

import Distances

partOne = do
  input <- readFile "input.txt"
  let edges = parseEdges input
  print edges
  print $ shortestPath edges

main = partOne
