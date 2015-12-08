module Circuit where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)
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
          | LShift Value Value
          | RShift Value Value
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
  op <- choice [ string "AND"
               , string "OR"
               , string "LSHIFT"
               , string "RSHIFT"
               ]
  v2 <- value
  case op of
    "AND" -> return $ And v1 v2
    "OR" -> return $ Or v1 v2
    "LSHIFT" -> return $ LShift v1 v2
    "RSHIFT" -> return $ RShift v1 v2
    _ -> fail "No operation matched"

eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return ()
