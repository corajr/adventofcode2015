module Flight where

import Text.Regex.PCRE
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.RWS.Strict
import Control.Monad (replicateM_, forM_)

type Reindeer = String

data Constraint = Constraint
           { name :: Reindeer
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

type Race = RWS [Constraint] () Distances ()

constraintRegex = "(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds."

parseConstraint :: String -> Constraint
parseConstraint l =
  let [_, s, k, f, m] = getAllTextSubmatches (l =~ constraintRegex :: (AllTextSubmatches [] String))
  in Constraint s (read k) (read f) (read m)

resetTime :: Constraint -> Distance -> Distance
resetTime c d = d { flightTimeLeft = fliesFor c
                  , restTimeLeft = mustRest c }

mtDistance = Distance 0 0 0

starting :: [Constraint] -> Distances
starting constraints = Map.fromList [(name x, f x) | x <- constraints]
  where f c = resetTime c mtDistance

step :: Race
step = do
  constraints <- ask
  forM_ constraints $ \c ->
    modify (Map.adjust (step1 c) (name c))

step1 :: Constraint -> Distance -> Distance
step1 c d
  | restTimeLeft d < 0 || flightTimeLeft d < 0 = error "Should not become negative"
  | restTimeLeft d == 1 = resetTime c d
  | flightTimeLeft d == 0 = d { restTimeLeft = restTimeLeft d - 1}
  | otherwise = d { flightTimeLeft = flightTimeLeft d - 1
                  , distance = distance d + kmPerSec c }

race :: Int -> [Constraint] -> Distances
race seconds constraints =
  let startState = starting constraints
  in fst $ execRWS (replicateM_ seconds step) constraints startState

maxDistance :: Int -> [Constraint] -> Int
maxDistance i c = maximum . map distance . Map.elems $ race i c
