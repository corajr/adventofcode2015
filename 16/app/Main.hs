module Main where

import Forensics

main :: IO ()
main = do
  input <- parseForensicsFromFile "input.txt"
  case input of
    Left err -> error (show err)
    Right sues -> print $ findSue partOneCriteria sues
