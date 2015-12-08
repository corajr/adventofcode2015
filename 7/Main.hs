module Main where

import Circuit
import Solve
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec (parseFromFile)

partOne circ =
  case circ of
    Left err -> fail (show err)
    Right circ' -> print $ Map.lookup "a" (solve circ')

main = do
  circ <- parseFromFile circuit "input.txt"
  partOne circ
