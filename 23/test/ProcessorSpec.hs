module ProcessorSpec (main, spec) where

import Test.Hspec
import Processor

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Processor" $ do
    context "when it gets the `inc` instruction" $
      it "should increment a register when it gets the `inc` instruction" $
        runProgram (program [ Increment A ]) `shouldBe` Registers { regA = 1, regB = 0 }
    context "when it gets the `hlf` instruction" $
      it "should halve a register" $
        runProgram (program [ Increment A
                            , Increment A
                            , Halve A ]) `shouldBe` Registers { regA = 1, regB = 0 }
    context "when it gets the `tpl` instruction" $
      it "should triple the register" $
        runProgram (program [ Increment A
                            , Triple A ]) `shouldBe` Registers { regA = 3, regB = 0 }
    context "when it gets the `jmp` instruction" $ do
      it "should jump forward by the specified amount" $
        runProgram (program [ Jump 2
                            , Jump 2
                            , Increment A ]) `shouldBe` Registers { regA = 1, regB = 0 }
      it "should jump backward by the specified amount" $
        runProgram (program [ Jump 2
                            , Jump 2
                            , Jump (-1)
                            , Increment A ]) `shouldBe` Registers { regA = 1, regB = 0 }
    context "when it gets the `jie a` instruction" $ do
      it "should jump if the register is even" $
        runProgram (program [ Increment A
                            , Increment A
                            , JumpIfEven A (-1)
                            ]) `shouldBe` Registers { regA = 3, regB = 0 }
      it "should not jump if the register is not even" $
        runProgram (program [ Increment A
                            , JumpIfEven A (-1)
                            ]) `shouldBe` Registers { regA = 1, regB = 0 }
    context "when it gets the `jio a, offset` instruction" $ do
      it "should jump if the register is 1" $
        runProgram (program [ Increment A
                            , JumpIfOne A (-1)
                            ]) `shouldBe` Registers { regA = 2, regB = 0 }
      it "should not jump if the register is not 1" $
        runProgram (program [ Increment A
                            , Increment A
                            , JumpIfOne A (-1)
                            ]) `shouldBe` Registers { regA = 2, regB = 0 }
