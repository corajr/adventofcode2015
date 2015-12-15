module CookieSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Cookie

main :: IO ()
main = hspec spec

sample1 = "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
sample2 = "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"

spec :: Spec
spec = do

  let ingredients = map parseIngredient [sample1, sample2]

  describe "parseIngredient" $
    it "parses a line into an Ingredient" $ do
      parseIngredient sample1 `shouldBe` Ingredient "Butterscotch" (-1) (-2) 6 3 8
      parseIngredient sample2 `shouldBe` Ingredient "Cinnamon" 2 3 (-2) (-1) 3
  describe "score" $
    it "should give 62842880 in the sample situation" $
      score ingredients [44, 56] `shouldBe` 62842880
  describe "maxScore" $
    it "should give 62842880 in the sample situation" $ do
      maxScore ingredients `shouldBe` 62842880
