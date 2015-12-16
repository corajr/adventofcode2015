module ForensicParser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import qualified Data.Map.Strict as Map

type Facts = Map.Map String Int

type SueNumber = Int

data Forensic = Forensic { sueNumber :: SueNumber
                         , knownFacts :: Facts }
              deriving (Show, Eq)

int :: GenParser Char st Int
int = rd <$> many1 digit
  where rd :: String -> Int
        rd = read

pSueNumber = string "Sue " *> int <* string ": "

pFact = do
  k <- many1 lower
  _ <- string ": "
  v <- int
  return (k, v)

pForensic :: GenParser Char st Forensic
pForensic = do
  sNo <- pSueNumber
  facts <- pFact `sepBy` (string ", ")
  _ <- (endOfLine *> pure ()) <|> eof
  return $ Forensic sNo (Map.fromList facts)

pForensics = many pForensic

parseForensic :: String -> Either ParseError Forensic
parseForensic = parse pForensic ""

parseForensics = parse pForensics ""

parseForensicsFromFile = parseFromFile pForensics
