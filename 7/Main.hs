module Main where

import Circuit
import Solve (solve)
import qualified Data.Map.Strict as Map
import Data.List (find)
import Text.ParserCombinators.Parsec (parseFromFile)

partOne circ = Map.lookup "a" (solve circ)

partTwo circ = Map.lookup "a" (solve circ')
  where b = find (\(Connection _ w) -> w == Wire "b") circ
        replaceB = Connection (V (Lit 16076)) (Wire "b")
        circ' = map (\x -> if Just x == b then replaceB else x) circ

main = do
  circ <- parseFromFile circuit "input.txt"
  case circ of
    Left err -> fail (show err)
    Right circ' -> print $ partTwo circ'
