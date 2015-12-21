module Medicine ( module Medicine.Parser
                , module Medicine
                ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.PQueue.Prio.Min as PQ
import Control.Parallel.Strategies
import Data.List (foldl')
import Text.EditDistance
import Debug.Trace (trace)
import Medicine.Parser

getDistinctMolecules :: Medicine -> Int
getDistinctMolecules = Set.size . getMolecules

getMolecules :: Medicine -> Set.HashSet Molecule
getMolecules med@(Medicine _ m) = Set.fromList $ do
  (atom, i) <- zip m [0..]
  to <- Map.lookupDefault [] atom (toSubMap med)
  let (xs, _:ys) = splitAt i m
  return $ xs ++ to ++ ys

start = ["e"]

-- copy of https://gist.github.com/abhin4v/8172534

search :: Medicine -> Maybe Int
search med =
  astar (PQ.singleton (heuristic start) (start, 0))
        Set.empty (Map.singleton start 0)
  where
    heuristic x = if length x > length (molecule med) then error "search node longer than real molecule" else textHeuristic x
    textHeuristic x = levenshteinDistance defaultEditCosts (concat x) (concat $ molecule med)
    astar pq seen gscore
      | PQ.null pq = Nothing
      | node == molecule med = Just gcost
      | node `Set.member` seen = astar pq' seen gscore
      | otherwise = astar pq'' seen' gscore'
      where
        (node, gcost) = snd . PQ.findMin $ pq
        pq' = PQ.deleteMin pq
        seen' = Set.insert node seen
        successors = filter (\(s, g, _) -> not (s `Set.member` seen') &&
                                            (g < Map.lookupDefault maxBound s gscore))
                             successorsAndCosts
                             -- (trace (show successorsAndCosts) successorsAndCosts)
        pq'' = foldl' (\q (s, g, h) -> PQ.insert (g + h) (s, g) q) pq' successors
        gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors
        successorsAndCosts =
          parMap rdeepseq (\s -> (s, gcost + 1, heuristic s)) . Set.toList . getMolecules $ med {molecule = node}

