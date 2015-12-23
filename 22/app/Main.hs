module Main where

import RPG

bossStats = BS $ BossStats 71 10

main :: IO ()
main = do
  cheap <- cheapestWin bossStats
  print cheap
