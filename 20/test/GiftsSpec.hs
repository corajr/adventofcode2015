module GiftsSpec (main, spec) where

import Test.Hspec
import Gifts

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "gifts" $
    it "should return expected presents for the first 9 houses" $
      take 9 gifts `shouldBe` [10, 30, 40, 70, 60, 120, 80, 150, 130]
  describe "findHouse" $
    it "should return Just 6 for the first house over 100" $
      findHouse (> 100) `shouldBe` Just 6
