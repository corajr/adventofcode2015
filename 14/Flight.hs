module Flight where

import Text.Regex.PCRE
import qualified Data.Map.Strict as Map

type Reindeer = String

data Constraint = Constraint
           { subject :: Reindeer
           , kmPerSec :: Int
           , fliesFor :: Int
           , mustRest :: Int
           } deriving (Show, Eq)

type Distances = Map.Map String Int

constraintRegex = "(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds."

parseConstraint :: String -> Constraint
parseConstraint l =
  let [_, s, k, f, m] = getAllTextSubmatches (l =~ constraintRegex :: (AllTextSubmatches [] String))
  in Constraint s (read k) (read f) (read m)

race :: Int -> [Constraint] -> Distances
race seconds constraints = undefined

maxDistance :: Int -> [Constraint] -> Int
maxDistance i c = maximum . Map.elems $ race i c
