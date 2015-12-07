module Lights where

import Instructions
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

type Coords = (Integer, Integer)
type Lights s = STUArray s Coords Int

start :: ST s (Lights s)
start =
  let a = (0,0)
      b = (999,999)
  in newArray (a, b) 0

transformSTarray :: (Int -> Int) -> Region -> Lights s -> ST s (Lights s)
transformSTarray f (c1, c2) m = do
  forM_ (range (c1, c2)) $ \i -> do
    a <- readArray m i
    writeArray m i (f a)
  return m

modify :: Lights s -> Instruction -> ST s (Lights s)
modify m (Instruction task (c1,c2)) =
  case task of
    TurnOn -> transformSTarray (+ 1) (c1, c2) m
    TurnOff -> transformSTarray (\x -> if x >= 1 then x - 1 else 0) (c1, c2) m
    Toggle -> transformSTarray (+ 2) (c1, c2) m

leftOn :: [Instruction] -> Int
leftOn inst =
  let final = do {m <- start; foldM modify m inst >>= getElems}
  in sum (runST final)

