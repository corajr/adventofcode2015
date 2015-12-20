module MedicineSpec (main, spec) where

import Test.Hspec
import Medicine
import Medicine.ParserSpec (parsedMedicine)
import Text.ParserCombinators.Parsec (parse)

sampleBuild =
  unlines [ "e => H"
          , "e => O"
          , "H => HO"
          , "H => OH"
          , "O => HH"
          , ""
          , "HOH"
          ]

parsedBuild = case parse pMedicine "" sampleBuild of
  Left err -> error (show err)
  Right p -> p

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getDistinctMolecules" $ do
    it "should return 4 for HOH" $
      getDistinctMolecules parsedMedicine `shouldBe` 4
    it "should return 7 for HOHOHO" $ do
      let med = parsedMedicine { molecule = ["H", "O", "H", "O", "H", "O"]}
      getDistinctMolecules med `shouldBe` 7
  describe "search" $ do
    it "should take 3 steps to make HOH" $
      search parsedBuild `shouldBe` Just 3
    it "should take 6 steps to make HOHOHO" $ do
      let build = parsedBuild { molecule = ["H", "O", "H", "O", "H", "O"] }
      search build `shouldBe` Just 6
