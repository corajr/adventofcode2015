module RoboGrid where

import Grid hiding (main)
import Data.List (partition)

alternating :: [a] -> ([a], [a])
alternating lst =
  let indexed = zip lst [1..]
      (a,b) = partition (even . snd) indexed
  in (map fst a, map fst b)

main = do
  input <- readFile "input.txt"
  let (mv1, mv2) = alternating input
  let locations = doMoves mv1 ++ doMoves mv2
  print $ getNonZero locations

