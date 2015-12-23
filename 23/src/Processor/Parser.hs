module Processor.Parser where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM)
import Data.Vector

data Register = A
              | B
              deriving (Show, Eq)

type Offset = Int

data Instruction = Halve Register
                 | Triple Register
                 | Increment Register
                 | Jump Offset
                 | JumpIfEven Register Offset
                 | JumpIfOne Register Offset
                 deriving (Show, Eq)

pRegister :: GenParser Char st Register
pRegister = do
  reg <- string "a" <|> string "b"
  case reg of
    "a" -> return A
    "b" -> return B
    _ -> unexpected "invalid register"

pOffset :: GenParser Char st Offset
pOffset = do
  sign <- char '+' <|> char '-'
  let sgn = case sign of
              '+' -> 1
              '-' -> -1
              _ -> error "invalid sign"
  dig <- many1 digit
  return $ sgn * read dig

pRegOffset :: GenParser Char st (Register, Offset)
pRegOffset = do
  reg <- pRegister
  _ <- string ", "
  offset <- pOffset
  return (reg, offset)

pInstruction :: GenParser Char st Instruction
pInstruction = do
  instName <- many lower
  _ <- space
  case instName of
    "hlf" -> liftM Halve pRegister
    "tpl" -> liftM Triple pRegister
    "inc" -> liftM Increment pRegister
    "jmp" -> liftM Jump pOffset
    "jie" -> liftM (uncurry JumpIfEven) pRegOffset
    "jio" -> liftM (uncurry JumpIfOne) pRegOffset
    _ -> unexpected "invalid instruction"

pInstructions :: GenParser Char st (Vector Instruction)
pInstructions = fromList <$> pInstruction `endBy` newline <* eof

parse' :: String -> Either String (Vector Instruction)
parse' = f . parse pInstructions ""
  where f (Left err) = Left $ show err
        f (Right x) = Right x
