module Circuit where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Data.Word


data Wire = Wire { wireID :: String }
          deriving (Show, Eq)

data Value = W Wire
           | Lit Word16
           deriving (Show, Eq)

data Signal = V Value
            | G Gate
            deriving (Show, Eq)

data Gate = And Value Value
          | Or Value Value
          | LShift Value Int
          | RShift Value Int
          | Not Value
          deriving (Show, Eq)

data Connection = Connection Signal Wire
                deriving (Show, Eq)

type Circuit = [Connection]

circuit = many connection <* eof

connection :: GenParser Char st Connection
connection =
  Connection <$> signal
             <*  string " -> "
             <*> wire
             <* eol

lit = rd <$> many1 digit
  where rd :: String -> Word16
        rd = read

wire = Wire <$> many1 letter

value = (W <$> wire) <|>
        (Lit <$> lit)

signal = try (G <$> gate) <|>
         V <$> value

gate = notGate <|> binaryGate

notGate = do
  try (string "NOT")
  space
  v <- value
  return $ Not v

binaryGate = do
  v1 <- value
  space
  op <- choice [ try $ string "AND"
               , try $ string "OR"
               , try $ string "LSHIFT"
               , string "RSHIFT"
               ]
  space
  case op of
    "AND" -> liftM (And v1) value
    "OR" -> liftM (Or v1) value
    "LSHIFT" -> liftM (LShift v1 . fromIntegral) lit
    "RSHIFT" -> liftM (RShift v1 . fromIntegral) lit
    _ -> fail "No operation matched"

eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return ()
