module Medicine ( module Medicine.Parser
                , module Medicine
                ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Debug.Trace (trace)
import System.Random.Shuffle (shuffleM)
import System.Random (mkStdGen)
import Control.Monad.Random (evalRand)
import Medicine.Parser

type Substrings = Map.HashMap Molecule (Int, Int)

getDistinctMolecules :: Medicine -> Int
getDistinctMolecules = Set.size . getMolecules

getMolecules :: Medicine -> Set.HashSet Molecule
getMolecules med@(Medicine _ m) = Set.fromList $ do
  (atom, i) <- zip m [0..]
  to <- Map.lookupDefault [] atom (toSubMap med)
  let (xs, _:ys) = splitAt i m
  return $ xs ++ to ++ ys

substrings :: Int -> Molecule -> Substrings
substrings maxLen mole = Map.fromList $ do
  let n = length mole
  i <- [0..n-1]
  j <- [1.. (minimum [maxLen, n-i])]
  let substr = take j (drop i mole)
  return (substr, (i, j))

search :: Medicine -> Maybe Int
search med = evalRand (searchBack med) (mkStdGen 0)

searchBack origin = go 0 [""] (substitutes origin) origin
  where go i _ _ (Medicine _ ["e"]) = return $ Just i
        go i prior [] med@(Medicine subst mole)
          | mole == prior = redoOrder med >>= go i [""] subst
          | otherwise = go i prior subst med
        go i prior (x:xs) med@(Medicine subst mole) =
          let substrs = substrings (maximum . map (length . (\(Substitute _ mol) -> mol)) $ subst) mole
          in case applyTransform x substrs mole of
            Just newMole -> go (i + 1) mole xs (med { molecule = newMole } )
            Nothing -> go i mole xs med

trace' :: Show a => a -> a
trace' a = trace (show a) a

applyTransform :: Substitute -> Substrings -> Molecule -> Maybe Molecule
applyTransform (Substitute at mol) substrs mol'
  | at == "e" = if mol' == mol
                   then Just ["e"]
                   else Nothing
  | otherwise = fmap f (Map.lookup mol substrs)
  where f (i, j) = take i mol' ++ [at] ++ drop (i+j) mol'

redoOrder (Medicine subst m) =
  Medicine <$> shuffleM subst <*> pure m
