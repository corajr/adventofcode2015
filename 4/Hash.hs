{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (find)
import Data.Digest.Pure.MD5

zeros n = replicate n '0'

hashStartsWithZeros n x = take n (show (md5 x)) == zeros n

input = "yzbqklnj"

lst n = map (\(x,y) -> x `B.append` B.pack (show y)) (zip (repeat input) [n..])

partOne = print $ find (hashStartsWithZeros 5) (lst 1)

partTwo = print $ find (hashStartsWithZeros 6) (lst 282749)

main = partTwo
