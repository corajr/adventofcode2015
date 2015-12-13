module Seating where

import Text.Regex.PCRE
import Data.Array.Unboxed

type Person = String

data Datum = Datum
           { subject :: Person
           , change :: Int
           , object :: Person
           } deriving (Show, Eq)

data HappyInfo = HappyInfo [Person] ValueMatrix
          deriving (Show, Eq)

type ValueMatrix = UArray (Int, Int) Int

factRegex = "(\\w+) would (gain|lose) (\\d+) happiness units by sitting next to (\\w+)."

parseFact :: String -> Datum
parseFact l =
  let [_, s, g, c, o] = getAllTextSubmatches (l =~ factRegex :: (AllTextSubmatches [] String))
      mult = if g == "gain" then 1 else -1
  in Datum s (mult * (read c)) o

factsToInfo :: [Datum] -> HappyInfo
factsToInfo = undefined

parseValues :: String -> HappyInfo
parseValues = factsToInfo . map parseFact . lines

maxValue :: HappyInfo -> Int
maxValue _ = 0
