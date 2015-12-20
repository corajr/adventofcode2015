module MedicineSpec (main, spec) where

import Test.Hspec
import Medicine
import Medicine.ParserSpec (parsedMedicine)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "getDistinctMolecules" $ do
    it "should return 4 for HOH" $
      getDistinctMolecules parsedMedicine `shouldBe` 4
    it "should return 7 for HOHOHO" $ do
      let med = parsedMedicine { molecule = ["H", "O", "H", "O", "H", "O"]}
      getDistinctMolecules med `shouldBe` 7
