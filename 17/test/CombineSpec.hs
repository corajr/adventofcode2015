module CombineSpec where

import Test.Hspec
import Combine


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "combinations" $
    it "should give 4 for sample input" $
      combinations [20,15,10,5,5] 25 `shouldBe` 4
  describe "combinationsMinimum" $
    it "should give 2 for sample input" $
      combinationsMinimum [20,15,10,5,5] 25 `shouldBe` 3
