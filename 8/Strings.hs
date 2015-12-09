module Strings where

import Escapes
import Text.ParserCombinators.Parsec (parse)
import Text.Parsec.Error (ParseError)

partOne :: String -> Either ParseError Int
partOne input = do
  result <- parse pQuotedStrings "" input
  let xs = lines input
  let quotedLengths = sum (map length xs)
  return $ quotedLengths - sum (map (length . unQuoted) result)

partTwo input =
  let xs = lines input
      orig = sum (map length xs)
      new = sum (map (length . show) xs)
  in new - orig

main = do
  input <- readFile "input.txt"
  print $ partTwo input
