import Test.Hspec
import Circuit
import Control.Exception (evaluate)
import Text.ParserCombinators.Parsec (parse, parseFromFile)

simpleCircuit =
  unlines [ "123 -> x"
          , "456 -> y"
          , "x AND y -> d"
          , "x OR y -> e"
          , "x LSHIFT 2 -> f"
          , "y RSHIFT 2 -> g"
          , "NOT x -> h"
          , "NOT y -> i"
          ]

main :: IO ()
main = hspec $
  describe "Circuit" $ do
    it "should parse one gate" $
      parse connection "" "123 -> x\n" `shouldBe` Right (Connection (V (Lit 123)) (Wire "x"))
    it "should parse a simple circuit" $
      parse circuit "" simpleCircuit `shouldBe`
        Right [ Connection (V (Lit 123)) (Wire "x")
              , Connection (V (Lit 456)) (Wire "y")
              , Connection (G (And (W (Wire "x")) (W (Wire "y")))) (Wire "d")
              , Connection (G (Or (W (Wire "x")) (W (Wire "y")))) (Wire "e")
              , Connection (G (LShift (W (Wire "x")) (Lit 2))) (Wire "f")
              , Connection (G (RShift (W (Wire "x")) (Lit 2))) (Wire "g")
              , Connection (G (Not (W (Wire "x")))) (Wire "h")
              , Connection (G (Not (W (Wire "x")))) (Wire "i")
              ]
    it "should parse a file" $ do
      let circ = parseFromFile circuit "input.txt"
      circ >>= (`shouldSatisfy` (\x -> length x == 8))
    it "should result in these signals" $ do
      pending
