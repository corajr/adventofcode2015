module Main2 where

import Instructions
import Lights
import Text.ParserCombinators.Parsec (parseFromFile)

main = do
  input <- parseFromFile instructions "input.txt"
  case input of
    Left err -> print err
    Right inst -> print $ leftOn inst
