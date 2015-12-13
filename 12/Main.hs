module Main where

import qualified Data.ByteString.Lazy as B
import Abacus

main = do
  input <- B.readFile "input.json"
  print $ sum (getNumbers input)
