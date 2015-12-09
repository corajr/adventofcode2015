module Escapes where

import Text.ParserCombinators.Parsec
import Numeric (readHex)
import Data.Char (chr)
import Data.Maybe (listToMaybe)

newtype QuotedString = QuotedString { unQuoted :: String }

pQuotedStrings = (pQuotedString `endBy` char '\n') <* eof

pQuotedString = do
  char '"'
  s <- many pChar
  char '"'
  return (QuotedString s)

pChar = escapeChar <|> noneOf ['"']

liftReadS :: ReadS a -> String -> Parser a
liftReadS reader = maybe (unexpected "no parse") (return . fst) .
                   listToMaybe . filter (null . snd) . reader

twoHexDigits = do
  d <- count 2 hexDigit
  liftReadS readHex d

escapeChar = do
  try (char '\\')
  next <- choice [ try (char '\\')
                 , try (char '"')
                 , char 'x'
                 ]
  case next of
    '\\' -> return '\\'
    '"' -> return '"'
    'x' -> do
      h <- twoHexDigits
      return (chr h)
    _ -> fail "invalid escape"
