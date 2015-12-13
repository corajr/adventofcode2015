module Main where

import Data.List (group)

input = 1321131112

lookAndSay :: String -> String
lookAndSay = concatMap say . group
  where say lst@(x:_) = show (length lst) ++ [x]

partOne = length $ (iterate lookAndSay (show input)) !! 40

main = print partOne
