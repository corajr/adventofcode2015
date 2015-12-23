module Processor.ParserSpec (main, spec, sampleProgram) where

import Test.Hspec
import Processor.Parser
import Text.ParserCombinators.Parsec
import Text.Parsec.Error (ParseError)
import Data.Either (isLeft)
import Data.Vector (fromList)


main :: IO ()
main = hspec spec

sampleString =
  unlines [ "inc a"
          , "jio a, +2"
          , "tpl a"
          , "inc a"
          ]

sampleProgram = fromList [ Increment A
                         , JumpIfOne A 2
                         , Triple A
                         , Increment A
                         ]

parseEOF :: GenParser Char () a -> String -> Either ParseError a
parseEOF p = parse (p <* eof) ""

spec :: Spec
spec = do
  describe "pRegister" $ do
    it "should parse register names" $ do
      parseEOF pRegister "a" `shouldBe` Right A
      parseEOF pRegister "b" `shouldBe` Right B
    it "should not parse more than the register" $
      parseEOF pRegister "b," `shouldSatisfy` isLeft
  describe "pOffset" $ do
    it "should parse offsets" $ do
      parseEOF pOffset "+2" `shouldBe` Right 2
      parseEOF pOffset "-4" `shouldBe` Right (-4)
    it "should not parse more than the offset" $
      parseEOF pRegister "+2\n" `shouldSatisfy` isLeft
  describe "pRegOffset" $ do
    it "should parse the register and the offset" $ do
      parseEOF pRegOffset "a, +3" `shouldBe` Right (A, 3)
      parseEOF pRegOffset "b, -3" `shouldBe` Right (B, -3)
    it "should parse the register and the offset" $
      parseEOF pRegOffset "a, +3\n" `shouldSatisfy` isLeft
  describe "pInstruction" $ do
    it "should parse a single instruction" $ do
      parseEOF pInstruction "inc a" `shouldBe` Right (Increment A)
      parseEOF pInstruction "jio b, -4" `shouldBe` Right (JumpIfOne B (-4))
    it "should parse no more than a single instruction" $
      parseEOF pInstruction "inc a\ninc b" `shouldSatisfy` isLeft
  describe "parse'" $
    it "should parse a sample program" $
      parse' sampleString `shouldBe` Right sampleProgram
