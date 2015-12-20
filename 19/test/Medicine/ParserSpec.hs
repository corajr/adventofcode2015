module Medicine.ParserSpec (main, spec, sampleMedicine) where

import Test.Hspec
import Medicine.Parser
import Text.ParserCombinators.Parsec
import Data.Either (isLeft)

sampleMedicine :: String
sampleMedicine =
  unlines [ "H => HO"
          , "H => OH"
          , "O => HH"
          , ""
          , "HOH"
          ]

parsedMedicine = Medicine subst m
  where subst = [ Substitute "H" ["H", "O"]
                , Substitute "H" ["O", "H"]
                , Substitute "O" ["H", "H"]
                ]
        m = ["H", "O", "H"]

main :: IO ()
main = hspec spec

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse (p <* eof) ""

spec :: Spec
spec = do
  describe "pAtom" $ do
    it "should parse an atom" $ do
      parse' pAtom "H" `shouldBe` Right "H"
      parse' pAtom "He" `shouldBe` Right "He"
    it "should not parse a molecule" $ do
      parse' pAtom "HO" `shouldSatisfy` isLeft
      parse' pAtom "HeO" `shouldSatisfy` isLeft
  describe "pMolecule" $
    it "should parse molecules" $ do
      parse' pMolecule "HO" `shouldBe` Right ["H", "O"]
      parse' pMolecule "HeO" `shouldBe` Right ["He", "O"]
      parse' pMolecule "CaOH" `shouldBe` Right ["Ca", "O", "H"]
  describe "pMedicine" $
    it "should parse the example" $
      parse' pMedicine sampleMedicine `shouldBe` Right parsedMedicine

