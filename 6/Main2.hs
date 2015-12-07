module Main2 where

import Instructions
import Data.Array.Unboxed
import Text.ParserCombinators.Parsec (parseFromFile)

type Lights = UArray (Integer, Integer) Int

start :: Lights
start =
  let a = (0,0)
      b = (999,999)
  in array (a, b) [(ix, 0) | ix <- range (a,b) ]

modify :: Lights -> Instruction -> Lights
modify m (Instruction task (c1,c2)) =
  let assoc = [(ix, 0) | ix <- range (c1,c2)]
  in case task of
    TurnOn -> accum (\x _ -> x + 1) m assoc
    TurnOff -> accum (\x _ -> if x >= 1 then x - 1 else 0) m assoc
    Toggle -> accum (\x _ -> x + 2) m assoc

leftOn :: [Instruction] -> Int
leftOn inst =
  let final = foldl modify start inst
  in sum (elems final)

main = do
  input <- parseFromFile instructions "input.txt"
  case input of
    Left err -> print err
    Right inst -> print $ leftOn inst
