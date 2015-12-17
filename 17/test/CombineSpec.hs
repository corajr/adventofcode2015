module CombineSpec where

import Test.Hspec
import Combine


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "combinations" $
    it "should give 4 for sample input" $
      combinations [20,15,10,5,5] 25 `shouldBe` 4
