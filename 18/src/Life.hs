module Life where

import Data.Array.Unboxed

type Grid = UArray (Int, Int) Bool

parseGrid :: String -> Grid
parseGrid input =
  let ls = lines input
      f x = case x of
        '#' -> True
        _ -> False
      m = length ls
      n = length (head ls)
      allChars = concat ls
  in listArray ((1, 1), (m, n)) (map f allChars)

step :: Grid -> Grid
step = undefined

steps :: Int -> String -> Grid
steps n gridS =
  let grid = parseGrid gridS
  in foldl (\acc _ -> step acc) grid [1..n]

lightsOn :: Grid -> Int
lightsOn = length . filter id . elems
