module Instructions where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, (<|>))

type Coordinate = (Integer, Integer)

type Region = (Coordinate, Coordinate)

data Instruction = Instruction Task Region
                   deriving (Show)

data Task = TurnOn | Toggle | TurnOff
          deriving (Show)

instructions = many instruction <* eof

instruction :: GenParser Char st Instruction
instruction =
  Instruction <$> task
              <* space
              <*> region
              <* eol

task = try (TurnOn <$ string "turn on") <|>
       try (TurnOff <$ string "turn off") <|>
       (Toggle <$ string "toggle")

region = (,) <$> coord
             <* string " through "
             <*> coord

coord = (,) <$> integer
        <* char ','
        <*> integer

integer = rd <$> many1 digit
    where rd = read :: String -> Integer


eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return ()

