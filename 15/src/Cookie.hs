module Cookie where

import Text.Regex.PCRE
import Data.List (transpose)
import Control.Monad (replicateM)

data Ingredient =
  Ingredient { name :: String
             , capacity :: Integer
             , durability :: Integer
             , flavor :: Integer
             , texture :: Integer
             , calories :: Integer
             } deriving (Show, Eq)

ingredientRegex = "([^:]+): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)"

parseIngredient :: String -> Ingredient
parseIngredient x =
  let [_, n, c, d, f, t, cal] = getAllTextSubmatches (x =~ ingredientRegex :: AllTextSubmatches [] String)
  in Ingredient n (read c) (read d) (read f) (read t) (read cal)

score :: [Ingredient] -> [Integer] -> Integer
score ingredients teaspoons = product . map nonNegSum . transpose $ zipWith f ingredients teaspoons
  where f (Ingredient _ c d fl t _) i = map (i*) [c, d, fl, t]
        nonNegSum = (\x -> if x > 0 then x else 0). sum

allCombos :: Int -> [[Integer]]
allCombos n = [ xs | xs <- replicateM n [0..100], sum xs == 100]

maxScore :: [Ingredient] -> Integer
maxScore ingredients =
  let n = length ingredients
  in maximum $ map (score ingredients) (allCombos n)
