{-# LANGUAGE ScopedTypeVariables #-}

module MultiDijkstra
  ( Grammar(..), Expansion(..)
  , Tree(..)
  , dijkstra
  ) where

import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.PSQueue (PSQ, Binding((:->)))
import qualified Data.PSQueue as PSQ
import Data.Set (Set)
import qualified Data.Set as Set


newtype Grammar a = Grammar (Map String (Set [Expansion a]))
data Expansion a = Term a | NonTerm String deriving (Show)

data Tree a = Node String [Tree a] | Leaf (Expansion a) deriving (Show)


instance Eq a => Eq (Expansion a) where
  (Term    a) == (Term    b) = a == b
  (NonTerm a) == (NonTerm b) = a == b
  _ == _ = False

instance Ord a => Ord (Expansion a) where
  compare (Term    a) (Term    b) = compare a b
  compare (NonTerm a) (NonTerm b) = compare a b
  compare (Term    _) (NonTerm _) = GT
  compare (NonTerm _) (Term    _) = LT


dijkstra :: forall w a. (Num w, Ord w, Ord a)
         => Grammar a
         -> ((String, [Expansion a]) -> w)
         -> [(w, Expansion a)]
         -> Set (Expansion a)
         -> Maybe (w, Tree a)
dijkstra (Grammar g) weight start end = go initialFringe Map.empty Map.empty
  where
    initialFringe :: PSQ (Expansion a) w
    initialFringe = PSQ.fromList $ map (\(w, s) -> s :-> w) start

    edges :: EdgeList a
    edges = reverseLookup $ Grammar g

    go :: PSQ (Expansion a) w
       -> Map String [Expansion a]
       -> Map (Expansion a) w
       -> Maybe (w, Tree a)
    go fringe parents done = case PSQ.minView fringe of
      Nothing -> Nothing
      (Just result) -> uncurry finishNode result
      where
        finishNode :: Binding (Expansion a) w
                   -> PSQ (Expansion a) w
                   -> Maybe (w, Tree a)
        finishNode (node :-> w) smallerFringe
          | Set.member node end = Just (w, buildTree parents node)
          | otherwise = go newFringe newParents newDone
          where
            newDone :: Map (Expansion a) w
            newDone = Map.insert node w done

            (newFringe, newParents) = foldl' updateDestination (smallerFringe, parents) myEdges

            myEdges :: [(String, [Expansion a])]
            myEdges = fromMaybe [] $ Map.lookup node edges

            updateDestination :: (PSQ (Expansion a) w, Map String [Expansion a])
                              -> (String, [Expansion a])
                              -> (PSQ (Expansion a) w, Map String [Expansion a])
            updateDestination (f, p) (destName, sources)
              | isNothing newWDest = (f, p)
              | isNothing wDest || newWDest < wDest = (f', p')
              | otherwise = (f, p)
              where
                wDest = PSQ.lookup (NonTerm destName) f
                newWDest = fmap sum . sequence $ map (`Map.lookup` newDone) sources
                f' = PSQ.insert (NonTerm destName) (fromJust newWDest) f
                p' = Map.insert destName sources p


type EdgeList a = Map (Expansion a) [(String, [Expansion a])]

reverseLookup :: forall a. Ord a => Grammar a -> EdgeList a
reverseLookup (Grammar g) = foldl' processRuleSet Map.empty (Map.toList g)
  where
    processRuleSet :: EdgeList a -> (String, Set [Expansion a]) -> EdgeList a
    processRuleSet edges (name, expansions) = Set.foldl' processExpansion edges expansions
      where
        processExpansion es expansion = processRule es (name, expansion)

    processRule :: EdgeList a -> (String, [Expansion a]) -> EdgeList a
    processRule edges rule@(name, expansion) = foldl' processSymbol ensureNT expansion
      where
        ensureNT = Map.alter (Just . fromMaybe []) (NonTerm name) edges
        processSymbol = flip $ Map.alter $ Just . (rule :) . fromMaybe []


buildTree :: Map String [Expansion a] -> Expansion a -> Tree a
buildTree _ e@(Term _) = Leaf e
buildTree parents e@(NonTerm name) = case Map.lookup name parents of
  Nothing -> Leaf e
  (Just children) -> Node name $ map (buildTree parents) children
