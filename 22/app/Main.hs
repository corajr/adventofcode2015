module Main where

import RPG

bossStats = BossStats 71 10

main :: IO ()
main = print $ cheapestWin bossStats
