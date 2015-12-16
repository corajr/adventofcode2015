module ForensicsSpec (main, spec) where

import Test.Hspec
import Data.Either (rights, isRight)
import Forensics
import qualified Data.Map.Strict as Map

sample1 = "Sue 1: cars: 9, akitas: 3, goldfish: 0"
sample2 = "Sue 2: akitas: 9, children: 3, samoyeds: 9"

sample1parsed = Forensic 1 vals
  where vals = Map.fromList [ ("cars", 9)
                            , ("akitas", 3)
                            , ("goldfish", 0)
                            ]

sample2parsed = Forensic 2 vals
  where vals = Map.fromList [ ("akitas", 9)
                            , ("children", 3)
                            , ("samoyeds", 9)
                            ]


criteria = Map.fromList [ ("akitas", 3)
                        , ("vizslas", 5)
                        ]

samples = rights $ map parseForensic [sample1, sample2]

main = hspec spec

spec :: Spec
spec = do
  describe "parseForensic" $
    it "should parse a sample" $
      parseForensic sample1 `shouldBe` Right sample1parsed
  describe "parseForensics" $
    it "should parse two samples" $
      parseForensics (unlines [sample1, sample2]) `shouldBe` Right [sample1parsed, sample2parsed]
  describe "parseForensicsFromFile" $
    it "should parse a whole file" $
      parseForensicsFromFile "input.txt" >>= (`shouldSatisfy` isRight)
  describe "findSue" $
    it "should find a Sue matching the criteria" $
      findSue criteria samples `shouldBe` Just 1
