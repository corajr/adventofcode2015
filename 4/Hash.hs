{-# LANGUAGE OverloadedStrings #-}
module Hash where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (find)
import Data.Digest.Pure.MD5

hashStartsWithZeros x = take 5 (show (md5 x)) == "00000"

input = "yzbqklnj"

lst = map (\(x,y) -> x `B.append` B.pack (show y)) (zip (repeat input) [1..])

main = print $ find hashStartsWithZeros lst
