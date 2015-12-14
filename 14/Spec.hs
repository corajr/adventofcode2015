module Main where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Control.Arrow (second)
import Flight

sample1 = "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
sample2 = "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."

mapValues :: (v1 -> v2) -> Map.Map k v1 -> Map.Map k v2
mapValues f = Map.fromDistinctAscList . map (second f) . Map.toAscList

getDists = mapValues distance

main = hspec $ do
  describe "parseConstraint" $
    it "should parse single constraints" $ do
      parseConstraint sample1 `shouldBe` Constraint "Comet" 14 10 127
      parseConstraint sample2 `shouldBe` Constraint "Dancer" 16 11 162

  let constraints = map parseConstraint [sample1, sample2]
  describe "race" $ do
    it "after 1 second, should give Comet: 14, Dancer: 16" $
      getDists (race 1 constraints) `shouldBe` Map.fromList [("Comet", 14), ("Dancer", 16)]
    it "after 10 seconds, should give Comet: 140, Dancer: 160" $
      getDists (race 10 constraints) `shouldBe` Map.fromList [("Comet", 140), ("Dancer", 160)]
    it "after 11 seconds, should give Comet: 140, Dancer: 176" $
      getDists (race 11 constraints) `shouldBe` Map.fromList [("Comet", 140), ("Dancer", 176)]
    it "after 138 seconds, should give Comet: 140, Dancer: 176" $
      getDists (race 138 constraints) `shouldBe` Map.fromList [("Comet", 140), ("Dancer", 176)]
    it "after 148 seconds, should give Comet: 280, Dancer: 176" $
      getDists (race 148 constraints) `shouldBe` Map.fromList [("Comet", 280), ("Dancer", 176)]
    it "after 174 seconds, should give Comet: 280, Dancer: 176" $
      getDists (race 174 constraints) `shouldBe` Map.fromList [("Comet", 280), ("Dancer", 176)]
    it "after 185 seconds, should give Comet: 280, Dancer: 352" $
      getDists (race 185 constraints) `shouldBe` Map.fromList [("Comet", 280), ("Dancer", 352)]
  describe "maxDistance" $
      it "should give 1120 in the sample situation" $
        maxDistance 1000 constraints `shouldBe` 1120
