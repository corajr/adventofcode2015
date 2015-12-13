{-# LANGUAGE OverloadedStrings #-}

module Abacus where

import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as M

getNumbers :: B.ByteString -> [Integer]
getNumbers = getNums . fromMaybe Null . decode
  where getNums :: Value -> [Integer]
        getNums (Number v) = [floor v]
        getNums (Array xs) = concatMap getNums xs
        getNums (Object o) = let vs = M.elems o
                             in if "red" `elem` vs then [] else concatMap getNums vs
        getNums _ = []
