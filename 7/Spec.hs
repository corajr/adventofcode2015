import Test.Hspec
import Circuit
import Solve
import Text.ParserCombinators.Parsec (parse, parseFromFile)
import qualified Data.Map.Strict as Map

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

circSolution =
  Map.fromList [ ("d", 72)
               , ("e", 507)
               , ("f", 492)
               , ("g", 114)
               , ("h", 65412)
               , ("i", 65079)
               , ("x", 123)
               , ("y", 456)
               ]

main :: IO ()
main = hspec $ do
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
              , Connection (G (RShift (W (Wire "y")) (Lit 2))) (Wire "g")
              , Connection (G (Not (W (Wire "x")))) (Wire "h")
              , Connection (G (Not (W (Wire "y")))) (Wire "i")
              ]
    it "should parse a file" $ do
      let circ = parseFromFile circuit "input.txt"
      circ >>= (\e ->
        case e of
          Left err -> error (show err)
          Right xs -> return xs)
        >>= (`shouldSatisfy` (\xs -> length xs == 339))
  describe "solve" $
    it "should result in the following signals for a simple circuit" $ do
      let eCirc = parse circuit "" simpleCircuit
      case eCirc of
        Right circ -> solve circ `shouldBe` circSolution
