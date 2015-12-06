module Present where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, (<|>))

data Present = Present Integer Integer Integer
             deriving Show

integer = rd <$> many1 digit
    where rd = read :: String -> Integer

eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return ()

presents = many present <* eof

present :: GenParser Char st Present
present =
  Present <$> integer
          <* char 'x'
          <*> integer
          <* char 'x'
          <*> integer
          <* eol
