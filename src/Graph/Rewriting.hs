module Graph.Rewriting where

import Graph.Digraph
import Graph.TypedDigraph

type Rule a b = Morphism a b

findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Morphism a b]
findMatches = undefined

-- rewrite rule graph match
rewrite :: (Monad m) => Rule a b -> TypedDigraph a b -> Morphism a b -> m (TypedDigraph a b)
rewrite = undefined

renameElementsByMatch :: (Eq a, Eq b) => Rule a b -> Morphism a b -> Rule a b
renameElementsByMatch r@(Morphism nr er) m@(Morphism nm em) = Morphism (filter newNames nr) (filter newNames er)
	where
		newNames, renames :: Eq a => (Maybe a, b) -> Bool
		newNames = (== Nothing) . fst
		renames  = (/= Nothing) . fst