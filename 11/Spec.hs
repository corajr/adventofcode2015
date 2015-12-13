module Main where

import Test.Hspec
import Password

main = hspec $ do
  describe "straight" $ do
    it "should accept `hijklmmn`" $
      "hijklmmn" `shouldSatisfy` straight
    it "should not accept `abbceffg`" $
      "abbceffg" `shouldSatisfy` not . straight
  describe "unambiguous" $ do
    it "should accept passwords without i, o, l" $ do
      "habcdefg" `shouldSatisfy` unambiguous
      "mnpqrstv" `shouldSatisfy` unambiguous
    it "should reject passwords with i, o, l" $ do
      "hijklmmn" `shouldSatisfy` not . unambiguous
      "hojkwmmn" `shouldSatisfy` not . unambiguous
  describe "twoPairs" $ do
    it "should accept `abbceffg`" $
      "abbceffg" `shouldSatisfy` twoPairs
    it "should reject `abbcegjk`" $
      "abbcegjk" `shouldSatisfy` not . twoPairs
  describe "requirements" $ do
    it "should check that all requirements are fulfilled" $ do
      "abcdffaa" `shouldSatisfy` requirements
      "ghjaabcc" `shouldSatisfy` requirements
    it "should reject invalid passwords" $ do
      "hijklmmn" `shouldSatisfy` not . requirements
      "abbceffg" `shouldSatisfy` not . requirements
      "abbcegjk" `shouldSatisfy` not . requirements
