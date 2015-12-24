module Main where

import Quantum

main :: IO ()
main = do
  input <- readFile "input.txt"
  let nums = map read $ lines input
  print $ bestEntanglement nums
