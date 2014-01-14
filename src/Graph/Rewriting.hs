module Graph.Rewriting where

import Graph.Digraph
import Graph.TypedDigraph

type Rule a b = Morphism a b a b

findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Endomorphism a b]
findMatches = undefined

-- rewrite rule graph match
rewrite :: Rule a b -> TypedDigraph a b -> Endomorphism a b -> TypedDigraph a b
rewrite = undefined
-- fmap (rewrite rule graph) matches