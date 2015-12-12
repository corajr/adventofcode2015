module Distances where

import Text.Regex.PCRE

type Vertex = String
data Edge = Edge Vertex Vertex Int
          deriving Show

lineReg = "(\\w+) to (\\w+) = (\\d+)"

parse :: String -> Edge
parse l =
  let [_, p1, p2, d] = getAllTextSubmatches (l =~ lineReg :: (AllTextSubmatches [] String))
  in Edge p1 p2 (read d)

parseEdges :: String -> [Edge]
parseEdges = map parse . lines

shortestPath :: [Edge] -> Int
shortestPath dists = undefined
