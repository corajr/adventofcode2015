module DiagonalSpec (main, spec) where

import Test.Hspec
import Diagonal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "nFromCoord" $ do
    it "should return 1 for (1,1)" $
      nFromCoord 1 1 `shouldBe` 1
    it "should return 2 for (2,1)" $
      nFromCoord 2 1 `shouldBe` 2
    it "should return the correct rows" $ do
      map (nFromCoord 1) [1..6] `shouldBe` [1, 3, 6, 10, 15, 21]
      map (nFromCoord 2) [1..5] `shouldBe` [2, 5, 9, 14, 20]
      map (nFromCoord 3) [1..4] `shouldBe` [4, 8, 13, 19]
      map (nFromCoord 4) [1..3] `shouldBe` [7, 12, 18]
      map (nFromCoord 5) [1..2] `shouldBe` [11, 17]
      map (nFromCoord 6) [1..1] `shouldBe` [16]
  describe "getCode" $ do
    it "should return 20151125 at (1,1)" $
      getCode 1 1 `shouldBe` 20151125
    it "should return 31916031 at (2,1)" $
      getCode 2 1 `shouldBe` 31916031
    it "should return the values from the sample" $ do
      map (getCode 1) [1..6] `shouldBe` [20151125, 18749137, 17289845, 30943339, 10071777, 33511524]
      map (getCode 2) [1..6] `shouldBe` [31916031, 21629792, 16929656, 7726640, 15514188, 4041754]
      map (getCode 3) [1..6] `shouldBe` [16080970,  8057251,  1601130,  7981243, 11661866, 16474243]
      map (getCode 4) [1..6] `shouldBe` [24592653, 32451966, 21345942,  9380097, 10600672, 31527494]
      map (getCode 5) [1..6] `shouldBe` [77061, 17552253, 28094349,  6899651,  9250759, 31663883]
      map (getCode 6) [1..6] `shouldBe` [33071741,  6796745, 25397450, 24659492,  1534922, 27995004]

