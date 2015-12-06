module Grid where

import Control.Arrow ((&&&))
import Data.List

type Location = (Integer, Integer)

move :: Location -> Char -> Location
move (a, b) x = case x of
  '^' -> (a, b + 1)
  '>' -> (a + 1, b)
  'v' -> (a, b - 1)
  '<' -> (a - 1, b)
  _ -> (a,b)

doMoves :: String -> [Location]
doMoves = scanl move (0,0)

main = do
  input <- readFile "input.txt"
  let locations = doMoves input
      counts = map (head &&& length) $ group (sort locations)
      nonZero = filter (\(_,b) -> b > 0) counts
  print $ length nonZero

