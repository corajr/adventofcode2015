module Password where

import Text.Regex.PCRE
import Data.Char (ord, chr)
import Data.List (intercalate, mapAccumR, find)

findNextPassword input =
  find requirements (iterate inc input)

inc :: String -> String
inc = snd . mapAccumR f 1
  where f acc x | acc + ord x == ord 'z' + 1 = (1, 'a')
                | otherwise = (0, chr (acc + ord x))

requirements :: String -> Bool
requirements inp = all ($ inp) [straight, unambiguous, twoPairs]

triples = "(" ++ intercalate "|" (map makeTriple [ord 'a' .. ord 'x']) ++ ")"
  where makeTriple a = [chr a, chr (a + 1), chr (a + 2)]

straight :: String -> Bool
straight inp = inp =~ triples

unambiguous :: String -> Bool
unambiguous inp = not (inp =~ "(i|o|l)")

twoPairs :: String -> Bool
twoPairs inp = inp =~ "(.)\\1.*(.)\\2"
