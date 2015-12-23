module ProcessorSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Processor" $ do
    context "when it gets the `inc` instruction" $
      it "should increment a register when it gets the `inc` instruction" pending
    context "when it gets the `hlf` instruction" $
      it "should halve a register" pending
    context "when it gets the `tpl` instruction" $
      it "should triple the register" pending
    context "when it gets the `jmp` instruction" $ do
      it "should jump forward by the specified amount" pending
      it "should jump backward by the specified amount" pending
    context "when it gets the `jie a` instruction" $ do
      it "should jump if the register is even" pending
      it "should not jump if the register is not even" pending
    context "when it gets the `jio a, offset` instruction" $ do
      it "should jump if the register is 1" pending
      it "should not jump if the register is not 1" pending
