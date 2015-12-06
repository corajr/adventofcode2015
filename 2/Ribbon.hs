module Wrap where

import Text.ParserCombinators.Parsec (parseFromFile)
import Present

ribbon :: Present -> Integer
ribbon (Present l w h) =
  let dist = min (min (l+w) (l+h)) (w+h)
      vol = l * w * h
  in 2*dist + vol

main = do
  input <- parseFromFile presents "input.txt"
  case input of
    Left e -> print e
    Right pres -> print $ sum (map ribbon pres)


