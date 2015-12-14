module Flight where

import Text.Regex.PCRE
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

type Reindeer = String

data Constraint = Constraint
           { subject :: Reindeer
           , kmPerSec :: Int
           , fliesFor :: Int
           , mustRest :: Int
           } deriving (Show, Eq)

constraintRegex = "(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds."

parseConstraint :: String -> Constraint
parseConstraint l =
  let [_, s, k, f, m] = getAllTextSubmatches (l =~ constraintRegex :: (AllTextSubmatches [] String))
  in Constraint s (read k) (read f) (read m)

maxDistance :: Int -> [Constraint] -> Int
maxDistance seconds constraints = snd $ maximumBy (comparing snd) flights
  where flights = undefined :: [(String, Int)]
