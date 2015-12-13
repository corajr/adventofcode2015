module Distances where

import Text.Regex.PCRE
import Data.Array.Unboxed
import Data.List (elemIndex, nub, minimumBy, maximumBy, permutations)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

type Vertex = String
data Edge = Edge Vertex Vertex Int
          deriving Show

type IEdge = ((Int, Int), Int)

type DistanceMatrix = UArray (Int, Int) Int

data Graph = Graph [Vertex] DistanceMatrix
           deriving Show

lineReg = "(\\w+) to (\\w+) = (\\d+)"

parseEdge :: String -> Edge
parseEdge l =
  let [_, p1, p2, d] = getAllTextSubmatches (l =~ lineReg :: (AllTextSubmatches [] String))
  in Edge p1 p2 (read d)

getVertices = nub . concatMap (\(Edge p1 p2 _) -> [p1, p2])

edgesToIEdges verts edges =
  let lookUp v = fromMaybe (-1) (elemIndex v verts)
      f (Edge p1 p2 d) = ((lookUp p1, lookUp p2), d)
  in map f edges

revIEdge ((a, b), d) = ((b, a), d)

parse :: String -> Graph
parse input =
  let edges = map parseEdge (lines input)
      verts = getVertices edges
      n = length verts
      iEdges = edgesToIEdges verts edges
      dists = array ((0,0), (n - 1,n - 1)) (iEdges ++ map revIEdge iEdges)
  in Graph verts dists

window :: Int -> [a] -> [[a]]
window n lst@(_:xs)
  | length lst < n = []
  | otherwise = take n lst : window n xs


travelingSalesman :: Graph -> Int
travelingSalesman (Graph vs dists) =
  cost $ maximumBy (comparing cost) (permutations [0..length vs - 1])
  where cost :: [Int] -> Int
        cost lst = sum (map (\([a, b]) -> dists ! (a,b)) (window 2 lst))
