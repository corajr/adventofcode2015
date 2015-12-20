module Main where

import Medicine
import Text.ParserCombinators.Parsec (parseFromFile)

main :: IO ()
main = do
  input <- parseFromFile pMedicine "input.txt"
  case input of
    Left err -> error (show err)
    Right med -> print $ search med
