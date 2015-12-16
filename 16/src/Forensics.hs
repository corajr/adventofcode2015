module Forensics ( module ForensicParser
                 , module Forensics
                 ) where

import ForensicParser
import qualified Data.Map.Strict as Map
import Data.List (find)

partOneCriteria :: Facts
partOneCriteria =
  Map.fromList [ ("children", 3)
               , ("cats", 7)
               , ("samoyeds", 2)
               , ("pomeranians", 3)
               , ("akitas", 0)
               , ("vizslas", 0)
               , ("goldfish", 5)
               , ("trees", 3)
               , ("cars", 2)
               , ("perfumes", 1)
               ]

findSue :: Facts -> [Forensic] -> Maybe SueNumber
findSue facts = fmap sueNumber . find f
  where f (Forensic _ known) = and $ Map.elems (Map.intersectionWith (==) facts known)
