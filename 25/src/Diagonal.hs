module Diagonal where

import qualified Data.MemoCombinators as Memo
import Math.NumberTheory.Powers

getCode :: Integer -> Integer -> Integer
getCode i = getNthCode . nFromCoord i

nFromCoord :: Integer -> Integer -> Integer
nFromCoord = Memo.memo2 Memo.integral Memo.integral nFromCoord'
  where nFromCoord' 1 j = (j * (j+1)) `div` 2
        nFromCoord' i j = (j + (i-2)) + nFromCoord (i-1) j

getNthCode :: Integer -> Integer
getNthCode n = 20151125 * powerMod 252533 (n-1) 33554393 `mod` 33554393
