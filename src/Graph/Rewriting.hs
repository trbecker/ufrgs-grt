module Graph.Rewriting where

import Data.Maybe

import Graph.Digraph
import Graph.TypedDigraph

import Control.Monad
import Control.Arrow

type Rule a b = Morphism a b

findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Morphism a b]
findMatches = undefined

-- rewrite rule graph match
--rewrite :: (Monad m) => Rule a b -> TypedDigraph a b -> Morphism a b -> m (TypedDigraph a brewrite rule graph match = applyTypedMorphism (renameElementsByMatch rule match) graph

renameNode :: [(Int, Int)] -> Node a -> Node a
renameNode namemap (Node id x) = Node (fromJust $ lookup id namemap) x

renameEdge nnamemap enamemap (Edge id x y) = Edge (fromJust $ lookup id enamemap) 
	((fromJust . flip lookup nnamemap) *-> x) y

-- | Applies f to both elements in a tuple
(*->) :: (a -> b) -> (a, a) -> (b, b)
(*->) f = f *** f
 
renameElementsByMatch :: (Eq a, Eq b) => Rule a b -> Morphism a b -> Rule a b
renameElementsByMatch r@(Morphism nr er) m@(Morphism nm em) = Morphism 
	(map (liftM (renameNode nodeIdMap) *->) $ filter renames nr) 
	(map (liftM (renameEdge nodeIdMap edgeIdMap) *->) $ filter renames er)
	where
		nodeIdMap = map ((nodeId . fromJust) *->) nm
		edgeIdMap = map ((edgeId . fromJust) *->) em
		newNames, renames :: Eq a => (Maybe a, b) -> Bool
		newNames = (== Nothing) . fst
		renames  = (/= Nothing) . fst