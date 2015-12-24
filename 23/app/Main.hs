module Main where

import Processor
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  prog <- parseFromFile pInstructions "input.txt"
  case prog of
    Left err -> error (show err)
    Right prog' -> print $ runProgram prog'
