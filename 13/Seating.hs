module Seating where

import Text.Regex.PCRE
import Data.Array.Unboxed
import Data.List (nub, elemIndex)
import Data.Maybe (fromMaybe)

type Person = String

data Datum = Datum
           { subject :: Person
           , change :: Int
           , object :: Person
           } deriving (Show, Eq)

data HappyInfo = HappyInfo [Person] ValueMatrix
          deriving (Show, Eq)

type IEdge = ((Int, Int), Int)
type ValueMatrix = UArray (Int, Int) Int

factRegex = "(\\w+) would (gain|lose) (\\d+) happiness units by sitting next to (\\w+)."

parseFact :: String -> Datum
parseFact l =
  let [_, s, g, c, o] = getAllTextSubmatches (l =~ factRegex :: (AllTextSubmatches [] String))
      mult = if g == "gain" then 1 else -1
  in Datum s (mult * (read c)) o


parseValues :: String -> HappyInfo
parseValues = factsToInfo . map parseFact . lines

getPeople = nub . concatMap (\(Datum p1 _ p2) -> [p1, p2])

edgesToIEdges people edges =
  let lookUp p = fromMaybe (-1) (elemIndex p people)
      f (Datum p1 v p2) = ((lookUp p1, lookUp p2), v)
  in map f edges

factsToInfo :: [Datum] -> HappyInfo
factsToInfo edges =
  let verts = getPeople edges
      n = length verts
      iEdges = edgesToIEdges verts edges
      values = array ((0,0), (n - 1,n - 1)) iEdges
  in HappyInfo verts values

window :: Int -> [a] -> [[a]]
window n lst@(_:xs)
  | length lst < n = []
  | otherwise = take n lst : window n xs

maxValue :: HappyInfo -> Int
maxValue _ = 0
