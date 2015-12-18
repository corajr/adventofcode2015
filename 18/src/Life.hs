module Life where

import Data.Array.Unboxed

type Grid = UArray (Int, Int) Bool

parseGrid :: String -> Grid
parseGrid = undefined

step :: Grid -> Grid
step = undefined

steps :: Int -> String -> Grid
steps n gridS =
  let grid = parseGrid gridS
  in foldl (\acc _ -> step acc) grid [1..n]

lightsOn :: Grid -> Int
lightsOn = length . filter id . elems
