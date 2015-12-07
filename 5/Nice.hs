module Nice where

import Text.Regex.PCRE

vowels :: String -> Bool
vowels x = (x =~ "[aeiou]" :: Int) >= 3

noBadStrings :: String -> Bool
noBadStrings x = not (x =~ "(ab|cd|pq|xy)")

doubled :: String -> Bool
doubled x = x =~ "(.)\\1"

nice :: String -> Bool
nice x = noBadStrings x && vowels x && doubled x

main = do
  input <- readFile "input.txt"
  print $ length $ filter nice (lines input)
