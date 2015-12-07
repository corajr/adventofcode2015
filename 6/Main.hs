module Main where

import Instructions
import Data.Array
import Text.ParserCombinators.Parsec (parseFromFile)

type Lights = Array (Integer, Integer) Bool

start :: Lights
start =
  let a = (0,0)
      b = (999,999)
  in array (a, b) [(ix, False) | ix <- range (a,b) ]

modify :: Lights -> Instruction -> Lights
modify m (Instruction task (c1,c2)) =
  case task of
    TurnOn -> m//[(ix, True) | ix <- range (c1,c2)]
    TurnOff -> m//[(ix, False) | ix <- range (c1,c2)]
    Toggle -> accum (\x _ -> not x) m [(ix, False) | ix <- range (c1, c2)]

leftOn :: [Instruction] -> Int
leftOn inst =
  let final = foldl modify start inst
  in length $ filter id (elems final)

main = do
  input <- parseFromFile instructions "input.txt"
  case input of
    Left err -> print err
    Right inst -> print $ leftOn inst
