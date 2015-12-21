module Main where

import Gifts

main :: IO ()
main = print $ findHouse (> 36000000)
