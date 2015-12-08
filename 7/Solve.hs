module Solve where

import Circuit
import qualified Data.Map.Strict as Map
import Data.List (mapAccumL)
import Data.Maybe (catMaybes)
import Control.Monad
import Data.Word
import Data.Bits

type Solution = Map.Map String Word16

solve :: [Connection] -> Solution
solve = go Map.empty
  where go solution [] = solution
        go solution xs =
          let (solution', xs') = mapAccumL apply solution xs
          in go solution' (catMaybes xs')

apply :: Solution -> Connection -> (Solution, Maybe Connection)
apply solution conn@(Connection sig (Wire wire)) =
  case sig of
    V v -> maybeInsert (get v)
    G g -> maybeInsert (runGate g)
  where get = find solution
        maybeInsert v = case v of
          Just word -> (Map.insert wire word solution, Nothing)
          Nothing -> (solution, Just conn)
        runGate g' = case g' of
          And v1 v2 -> liftM2 (.&.) (get v1) (get v2)
          Or v1 v2 -> liftM2 (.|.) (get v1) (get v2)
          LShift v1 i -> liftM2 shiftL (get v1) (Just i)
          RShift v1 i -> liftM2 shiftR (get v1) (Just i)
          Not v1 -> liftM complement (get v1)

find :: Solution -> Value -> Maybe Word16
find solution value =
  case value of
    Lit l -> Just l
    W (Wire w) -> Map.lookup w solution
