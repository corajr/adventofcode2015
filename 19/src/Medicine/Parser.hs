module Medicine.Parser where

import Text.ParserCombinators.Parsec

type Atom = String

data Substitute = Substitute Atom Molecule
                deriving (Show, Eq)

type Molecule = [Atom]

data Medicine = Medicine
                { substitutes :: [Substitute]
                , molecule :: Molecule
                } deriving (Show, Eq)

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

