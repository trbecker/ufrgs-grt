module Graph.Rewriting where

import Data.Maybe
import Data.List

import Graph.Digraph
import Graph.TypedDigraph

import Control.Monad
import Control.Arrow

type Rule a b = Morphism a b

findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Morphism a b]
findMatches = undefined

-- rewrite rule graph match
--rewrite :: (Monad m) => Rule a b -> TypedDigraph a b -> Morphism a b -> m (TypedDigraph a brewrite rule graph match = applyTypedMorphism (rename rule match) graph

renameNode :: [(Int, Int)] -> Node a -> Node a
renameNode namemap (Node id x) = Node (fromJust $ lookup id namemap) x

renameEdge nnamemap enamemap (Edge id x y) = Edge (fromJust $ lookup id enamemap) 
	(double (fromJust . flip lookup nnamemap) x) y

-- | Applies f to both elements in a tuple
double :: (a -> b) -> (a, a) -> (b, b)
double f = f *** f
 
rename :: (Eq a, Eq b) => Int -> Int -> Rule a b -> Morphism a b -> Rule a b
rename ns es r@(Morphism nr er) m@(Morphism nm em) = Morphism 
	(nub $ map (double (liftM (renameNode nodeIdMap))) nr) 
	(nub $ map (double (liftM (renameEdge nodeIdMap edgeIdMap))) er)
	where
		cNodeIdMap = zip (map (nodeId . fromJust . snd) $ filter newNames nr) [ns..]
		cEdgeIdMap = zip (map (edgeId . fromJust . snd) $ filter newNames er) [es..]
		nodeIdMap = cNodeIdMap ++ map (double (nodeId . fromJust)) nm
		edgeIdMap = cEdgeIdMap ++ map (double (edgeId . fromJust)) em
		newNames, renames :: Eq a => (Maybe a, b) -> Bool
		newNames = (== Nothing) . fst
		renames  = (/= Nothing) . fst