module Wrap where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, (<|>))

data Present = Present Integer Integer Integer
             deriving Show

integer = rd <$> many1 digit
    where rd = read :: String -> Integer

eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return ()

presents = many present <* eof

present =
  Present <$> integer
          <* char 'x'
          <*> integer
          <* char 'x'
          <*> integer
          <* eol


wrap :: Present -> Integer
wrap (Present l w h) =
  let a = l * w
      b = w * h
      c = h * l
  in (2 * (a + b + c)) + min (min a b) c


main = do
  input <- parseFromFile presents "input.txt"
  case input of
    Left e -> print e
    Right pres -> print $ sum (map wrap pres)


