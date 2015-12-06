module Wrap where

import Text.ParserCombinators.Parsec (parseFromFile)
import Present

wrap :: Present -> Integer
wrap (Present l w h) =
  let a = l * w
      b = w * h
      c = h * l
  in (2 * (a + b + c)) + min (min a b) c

main = do
  input <- parseFromFile presents "input.txt"
  case input of
    Left e -> print e
    Right pres -> print $ sum (map wrap pres)


