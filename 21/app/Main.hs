module Main where

import RPG

bossStats = Stats 100 8 2

main :: IO ()
main = print $ cheapestWin bossStats
