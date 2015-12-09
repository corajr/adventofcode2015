module Strings where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding as E

partOne input = do
  let xs = lines input
  let unescaped = map (read :: String -> T.Text) xs
  print $ sum (map length xs) - sum (map T.length  unescaped)

main = do
  input <- readFile "input.txt"
  partOne input
