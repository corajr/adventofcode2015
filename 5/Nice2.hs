module Nice where

import Text.Regex.PCRE

doubled :: String -> Bool
doubled x = x =~ "(..).*\\1"

inBetween :: String -> Bool
inBetween x = x =~ "(.).\\1"

nice :: String -> Bool
nice x = doubled x && inBetween x

main = do
  input <- readFile "input.txt"
  print $ length $ filter nice (lines input)
