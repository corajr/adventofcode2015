module Processor ( module Processor
                 , module Processor.Parser) where

import Processor.Parser
import Data.Vector ((!), length)
import qualified Data.Vector as V
import Control.Monad (when)
import Control.Monad.Trans.RWS

data Registers = Registers { regA :: Int
                           , regB :: Int
                           } deriving (Show, Eq)

data ProgramState =
  ProgramState { programCounter :: Int
               , registers :: Registers
               } deriving (Show, Eq)

type Machine = RWS Program () ProgramState

mtRegisters :: Registers
mtRegisters = Registers { regA = 0
                        , regB = 0
                        }

mtState :: ProgramState
mtState = ProgramState { programCounter = 0
                       , registers = mtRegisters
                       }

execute :: Machine ()
execute = do
  pc <- gets programCounter
  inst <- asks (! pc)
  case inst of
    Halve reg -> modReg (`div` 2) reg >> modPC 1
    Triple reg -> modReg (*3) reg >> modPC 1
    Increment reg -> modReg (+1) reg >> modPC 1
    Jump i -> modPC i
    JumpIfEven reg i -> branch even reg i
    JumpIfOne reg i -> branch (== 1) reg i
  maybeContinue

branch :: (Int -> Bool) -> Register -> Int -> Machine ()
branch f reg i = do
  v <- getReg reg
  if f v
    then modPC i
    else modPC 1

getReg :: Register -> Machine Int
getReg reg = do
  regs <- gets registers
  case reg of
    A -> return $ regA regs
    B -> return $ regB regs

modReg :: (Int -> Int) -> Register -> Machine ()
modReg f reg = do
  regs <- gets registers
  let regs' = case reg of
                A -> regs { regA = f (regA regs) }
                B -> regs { regB = f (regB regs) }
  modify (\s -> s { registers = regs' })

maybeContinue :: Machine ()
maybeContinue = do
  pc <- gets programCounter
  n <- asks V.length
  when (pc < n) execute

modPC :: Int -> Machine ()
modPC i = modify (\ps -> ps { programCounter = programCounter ps + i })

runProgram :: Program -> Registers
runProgram r = registers . fst $ execRWS execute r mtState

program :: [Instruction] -> Program
program = V.fromList
