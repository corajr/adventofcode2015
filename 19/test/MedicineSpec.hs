module MedicineSpec (main, spec, parsedBuild) where

import Test.Hspec
import Medicine
import Medicine.ParserSpec (parsedMedicine)
import qualified Data.HashSet as Set
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

priorGenHOH =
  Set.fromList [ ["H", "H"]
               ]

priorGenHOHOHO =
  Set.fromList [ ["H", "O", "H", "O", "H"]
               , ["H", "H", "O", "H", "O"]
               , ["H", "O", "H", "H", "O"]
               ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let med = parsedMedicine { molecule = ["H", "O", "H", "O", "H", "O"]}
  describe "getDistinctMolecules" $ do
    it "should return 4 for HOH" $
      getDistinctMolecules parsedMedicine `shouldBe` 4
    it "should return 7 for HOHOHO" $
      getDistinctMolecules med `shouldBe` 7
  describe "getPriorGen" $ do
    let medH = parsedBuild { molecule = ["H"]}
    let medO = parsedBuild { molecule = ["O"]}
    let medHH = parsedBuild { molecule = ["H", "H"]}
    let medHO = parsedBuild { molecule = ["H", "O"]}
    let medOH = parsedBuild { molecule = ["O", "H"]}
    it "should return one step back for H" $
      getPriorGen medH `shouldBe` Set.fromList [["e"]]
    it "should return one step back for O" $
      getPriorGen medO `shouldBe` Set.fromList [["e"]]
    it "should return one step back for HH" $
      getPriorGen medHH `shouldBe` Set.fromList [["O"]]
    it "should return one step back for HO" $
      getPriorGen medHO `shouldBe` Set.fromList [["H"]]
    it "should return one step back for OH" $
      getPriorGen medOH `shouldBe` Set.fromList [["H"]]
    it "should return one step back for HOH" $
      getPriorGen parsedMedicine `shouldBe` priorGenHOH
    it "should return one step back for HOHOHO" $
      getPriorGen med `shouldBe` priorGenHOHOHO
  describe "buildInSteps" $ do
    it "should take 3 steps to make HOH" $
      buildInSteps parsedBuild `shouldBe` 3
    it "should take 6 steps to make HOHOHO" $ do
      let build = parsedBuild { molecule = ["H", "O", "H", "O", "H", "O"] }
      buildInSteps build `shouldBe` 6
