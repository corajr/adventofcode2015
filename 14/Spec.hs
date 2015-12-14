module Main where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Control.Arrow (second)
import Flight

sample1 = "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
sample2 = "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."

cometStart :: Distance
cometStart = Distance { distance = 0, flightTimeLeft = 10, restTimeLeft = 127 }

cometConstraint = parseConstraint sample1

inputConstraints = [Constraint {name = "Vixen", kmPerSec = 8, fliesFor = 8, mustRest = 53},Constraint {name = "Blitzen", kmPerSec = 13, fliesFor = 4, mustRest = 49},Constraint {name = "Rudolph", kmPerSec = 20, fliesFor = 7, mustRest = 132},Constraint {name = "Cupid", kmPerSec = 12, fliesFor = 4, mustRest = 43},Constraint {name = "Donner", kmPerSec = 9, fliesFor = 5, mustRest = 38},Constraint {name = "Dasher", kmPerSec = 10, fliesFor = 4, mustRest = 37},Constraint {name = "Comet", kmPerSec = 3, fliesFor = 37, mustRest = 76},Constraint {name = "Prancer", kmPerSec = 9, fliesFor = 12, mustRest = 97},Constraint {name = "Dancer", kmPerSec = 37, fliesFor = 1, mustRest = 36}]

mapValues :: (v1 -> v2) -> Map.Map k v1 -> Map.Map k v2
mapValues f = Map.fromDistinctAscList . map (second f) . Map.toAscList

getDists = mapValues distance

main = do
  input <- readFile "input.txt"
  hspec $ do
    describe "parseConstraint" $
      it "should parse single constraints" $ do
        parseConstraint sample1 `shouldBe` cometConstraint
        parseConstraint sample2 `shouldBe` Constraint "Dancer" 16 11 162
    describe "map parseConstraint" $ do
      let mapped = map parseConstraint (lines input)
      it "should parse an entire file" $
        mapped `shouldBe` inputConstraints
    let constraints = map parseConstraint [sample1, sample2]
    describe "resetTime" $ do
      it "should reset the flight and rest time on an empty Distance record" $
        resetTime cometConstraint mtDistance `shouldBe` cometStart
      it "should reset the flight and rest time on an updated Distance record" $ do
        let cometLater = cometStart { distance = 140, flightTimeLeft = 0 }
        resetTime cometConstraint cometLater `shouldBe` cometStart { distance = 140 }
    describe "step1" $ do
      let steps = iterate (step1 cometConstraint) cometStart
      it "should increase distance after 1 second" $
        steps !! 1 `shouldBe` cometStart { distance = 14, flightTimeLeft = 9 }
      it "should increase distance after 2 seconds" $
        steps !! 2 `shouldBe` cometStart { distance = 28, flightTimeLeft = 8 }
      it "should increase distance after 10 seconds" $
        steps !! 10 `shouldBe` cometStart { distance = 140, flightTimeLeft = 0 }
      it "should decrease restTime after 11 seconds" $
        steps !! 11 `shouldBe` cometStart { distance = 140, flightTimeLeft = 0, restTimeLeft = 126 }
      it "should reset times after 138 seconds" $
        steps !! 138 `shouldBe` cometStart { distance = 140 }
      it "should increase distance after 139 seconds" $
        steps !! 139 `shouldBe` cometStart { distance = 154, flightTimeLeft = 9, restTimeLeft = 127 }
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
      it "after 1000 seconds, should give Comet: 1120, Dancer: 1056" $
        getDists (race 1000 constraints) `shouldBe` Map.fromList [("Comet", 1120), ("Dancer", 1056)]
    describe "maxDistance" $ do
        it "should give 1120 in the sample situation" $
          maxDistance 1000 constraints `shouldBe` 1120
        it "should give 37 after 1 second with the input" $
          maxDistance 1 inputConstraints `shouldBe` 37
        it "should give something more than 2624 after 2503 seconds with the input" $
          maxDistance 2503 inputConstraints `shouldSatisfy` (\x -> x > 2624)
