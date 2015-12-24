module QuantumSpec where

import Test.Hspec
import Quantum
import qualified Data.Set as Set

main :: IO ()
main = hspec spec

samplePackages :: [Package]
samplePackages = [1..5] ++ [7..11]

spec :: Spec
spec = do
  describe "subsets" $
    it "should return all non-empty subsets" $
      subsets (Set.fromList "abc") `shouldBe` map Set.fromList ["a","b","ab","c","ac","bc","abc"]
  describe "arrangements" $
    it "should give back at least 1 possibility with {9, 11}" $
      arrangements samplePackages `shouldSatisfy` any ((==) (Set.fromList [9,11]) . firstGroup)
  describe "bestEntanglement" $
    it "should find the optimal arrangement of sample packages" $
      bestEntanglement samplePackages `shouldBe` 99
