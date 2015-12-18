module Main where

import Life

main :: IO ()
main = do
  input <- readFile "input.txt"
  let grid = steps 100 input
  print $ lightsOn grid
