module Password where

import Text.Regex.PCRE
import Data.Char (ord, chr)
import Data.List (intercalate)

inc :: String -> String
inc = undefined

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
