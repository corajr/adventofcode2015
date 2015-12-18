module LifeSpec where

import Test.Hspec
import Life
import Data.Array

start = unlines [ ".#.#.#"
                , "...##."
                , "#....#"
                , "..#..."
                , "#.#..#"
                , "####.."
                ]

oneStep = unlines [ "..##.."
                  , "..##.#"
                  , "...##."
                  , "......"
                  , "#....."
                  , "#.##.."
                  ]

twoStep = unlines [ "..###."
                  , "......"
                  , "..###."
                  , "......"
                  , ".#...."
                  , ".#...."
                  ]

threeStep = unlines [ "...#.."
                    , "......"
                    , "...#.."
                    , "..##.."
                    , "......"
                    , "......"
                    ]

fourStep = unlines [ "......"
                   , "......"
                   , "..##.."
                   , "..##.."
                   , "......"
                   , "......"
                   ]

startArray :: Grid
startArray =
  listArray ((1,1), (6,6))
    [ False, True, False, True, False, True
    , False, False, False, True, True, False
    , True, False, False, False, False, True
    , False, False, True, False, False, False
    , True, False, True, False, False, True
    , True, True, True, True, False, False
    ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseGrid" $
    it "should return an accurate grid from sample input" $
      parseGrid start `shouldBe` startArray

  let parsed = parseGrid start
  let pOneStep = parseGrid oneStep
  let pTwoStep = parseGrid twoStep
  let pThreeStep = parseGrid threeStep
  let pFourStep = parseGrid fourStep

  describe "countNeighbors" $ do
    let bound = bounds parsed
    it "should return 1 in the upper left corner of start" $
      countNeighbors bound parsed (1,1) `shouldBe` 1
    it "should return 2 in the lower left corner of start" $
      countNeighbors bound parsed (6,1) `shouldBe` 2
    it "should return 3 on the fourth sample step" $ do
      countNeighbors bound pFourStep (3,3) `shouldBe` 3
      countNeighbors bound pFourStep (3,4) `shouldBe` 3
      countNeighbors bound pFourStep (4,3) `shouldBe` 3
      countNeighbors bound pFourStep (4,4) `shouldBe` 3

  describe "step" $
    it "should step the grid once" $ do
      step parsed `shouldBe` pOneStep
      step pOneStep `shouldBe` pTwoStep
      step pTwoStep `shouldBe` pThreeStep
      step pThreeStep `shouldBe` pFourStep

  describe "steps" $
    it "should take the grid through n steps" $
      steps 4 start `shouldBe` pFourStep

  describe "lightsOn" $
    it "should count the number of lights" $ do
      lightsOn parsed `shouldBe` 15
      lightsOn pThreeStep `shouldBe` 4
      lightsOn pFourStep `shouldBe` 4

