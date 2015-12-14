module Flight where

import Text.Regex.PCRE
import qualified Data.Map.Strict as Map
import Data.List (maximumBy)
import Data.Ord (comparing)

type Reindeer = String

data Constraint = Constraint
           { subject :: Reindeer
           , kmPerSec :: Int
           , fliesFor :: Int
           , mustRest :: Int
           } deriving (Show, Eq)

data Distance = Distance
            { distance :: Int
            , flightTimeLeft :: Int
            , restTimeLeft :: Int
            } deriving (Show, Eq)

type Distances = Map.Map String Distance

constraintRegex = "(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds."

parseConstraint :: String -> Constraint
parseConstraint l =
  let [_, s, k, f, m] = getAllTextSubmatches (l =~ constraintRegex :: (AllTextSubmatches [] String))
  in Constraint s (read k) (read f) (read m)

race :: Int -> [Constraint] -> Distances
race seconds constraints = Map.empty

maxDistance :: Int -> [Constraint] -> Int
maxDistance i c = distance . maximumBy (comparing distance) . Map.elems $ race i c
