module Main where

import Password (findNextPassword)
input = "hepxcrrq"

partOne = findNextPassword input

partTwo = fmap findNextPassword $ findNextPassword input

main = print partTwo
