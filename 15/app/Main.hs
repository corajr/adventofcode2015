module Main where

import Cookie

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ingredients = map parseIngredient (lines input)
  print (maxScore ingredients)
