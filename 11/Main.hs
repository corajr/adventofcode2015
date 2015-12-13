module Main where

import Data.List (find)
import Password (inc, requirements)
input = "hepxcrrq"

partOne = find requirements (iterate inc input)

main = print partOne
