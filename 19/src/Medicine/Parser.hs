module Medicine.Parser where

import qualified Data.HashMap.Strict           as Map
import           Text.ParserCombinators.Parsec

class ToSubMap a where
  toSubMap :: a -> Map.HashMap Atom [Molecule]

type Atom = String

data Substitute = Substitute Atom Molecule
                deriving (Show, Eq)

type Molecule = [Atom]

data Medicine = Medicine
                { substitutes :: [Substitute]
                , molecule    :: Molecule
                } deriving (Show, Eq)

instance ToSubMap Medicine where
  toSubMap = substToMap . substitutes

pAtom = do
  a <- upper
  b <- many lower
  return $ a : b

pMolecule = many pAtom

pSubstitute = do
  atom <- choice [ try pAtom
                 , string "e" ]
  _ <- string " => "
  m <- pMolecule
  _ <- optional newline
  return $ Substitute atom m

pMedicine :: GenParser Char st Medicine
pMedicine = do
  subst <- many pSubstitute
  _ <- newline
  m <- pMolecule
  _ <- optional newline
  return $ Medicine subst m

substToMap :: [Substitute] -> Map.HashMap Atom [Molecule]
substToMap = foldl f Map.empty
  where f acc (Substitute a m) = Map.unionWith (++) acc (Map.singleton a [m])
