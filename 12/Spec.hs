{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Abacus

main = hspec $
  describe "getNumbers" $ do
    it "should get numbers from an empty array" $
      getNumbers "[]" `shouldBe` []
    it "should get numbers from an empty object" $
      getNumbers "{}" `shouldBe` []
    it "should get numbers from an array" $
      getNumbers "[1,2,3]" `shouldBe` [1,2,3]
    it "should get numbers from a simple object" $
      getNumbers "{\"a\":2,\"b\":4}" `shouldBe` [2,4]
    it "should get numbers from a nested array" $
      getNumbers "[[[3]]]" `shouldBe` [3]
    it "should get numbers from a nested array" $
      getNumbers "{\"a\":{\"b\":4},\"c\":-1}" `shouldBe` [4,-1]
