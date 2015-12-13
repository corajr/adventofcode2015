module Main where

import Test.Hspec
import Seating

sample1 = "Alice would gain 54 happiness units by sitting next to Bob."
sample2 = "Alice would lose 79 happiness units by sitting next to Carol."

sampleData =
  [ Datum {subject = "Alice", change = 54, object = "Bob"}
  , Datum {subject = "Alice", change = -79, object = "Carol"}
  , Datum {subject = "Alice", change = -2, object = "David"}
  , Datum {subject = "Bob", change = 83, object = "Alice"}
  , Datum {subject = "Bob", change = -7, object = "Carol"}
  , Datum {subject = "Bob", change = -63, object = "David"}
  , Datum {subject = "Carol", change = -62, object = "Alice"}
  , Datum {subject = "Carol", change = 60, object = "Bob"}
  , Datum {subject = "Carol", change = 55, object = "David"}
  , Datum {subject = "David", change = 46, object = "Alice"}
  , Datum {subject = "David", change = -7, object = "Bob"}
  , Datum {subject = "David", change = 41, object = "Carol"}
  ]

main = do
  test_input <- readFile "test_input.txt"
  hspec $ do
    describe "parseFact" $ do
      it "should parse a single fact" $ do
        parseFact sample1 `shouldBe` Datum "Alice" 54 "Bob"
        parseFact sample2 `shouldBe` Datum "Alice" (-79) "Carol"
      it "should, when mapped, parse a sample set of facts" $
        map parseFact (lines test_input) `shouldBe` sampleData
    describe "maxValue" $
      it "should give 330 for the sample arrangement" $ do
        let vs = parseValues test_input
        maxValue vs `shouldBe` 330
