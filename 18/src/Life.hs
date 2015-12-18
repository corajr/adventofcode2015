module Life where

import Data.Array

type Grid = Array (Int, Int) Bool
type NeighborGrid = Array (Int, Int) (Bool, Int)

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
step = stepFromNeighbor . toNeighbor

toNeighbor :: Grid -> NeighborGrid
toNeighbor grid = array (bounds grid) newAssocs
  where allAssocs = assocs grid
        bound = bounds grid
        newAssocs = map f allAssocs
        f (i, e) = (i, (e, countNeighbors bound grid i))

countNeighbors :: ((Int, Int), (Int, Int)) -> Grid -> (Int, Int) -> Int
countNeighbors bound grid (a,b) = sum . map boolNum $ map (grid !) others
  where others = filter rF $ range ((a - 1, b - 1), (a + 1, b + 1))
        rF x = x /= (a,b) && inRange bound x
        boolNum x = if x then 1 else 0

stepFromNeighbor :: NeighborGrid -> Grid
stepFromNeighbor = fmap f
  where f (True, 2) = True
        f (True, 3) = True
        f (True, _) = False
        f (False, 3) = True
        f (False, _) = False

steps :: Int -> String -> Grid
steps n gridS =
  let grid = parseGrid gridS
  in foldl (\acc _ -> step acc) grid [1..n]

lightsOn :: Grid -> Int
lightsOn = length . filter id . elems
