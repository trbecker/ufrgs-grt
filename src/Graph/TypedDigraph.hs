module Graph.TypedDigraph where

import Graph.Digraph

type TGraph = Digraph () ()

data Morphism a b a' b'= Morphism [(Maybe (Node a), Maybe (Node a'))] [(Maybe (Edge b), Maybe (Edge b'))]
data TypedDigraph a b = TypedDigraph (Digraph a b) TGraph (Morphism a b () ())

-- Endomorphism in the Graph a b category.
type Endomorphism a b = Morphism a b a b

applyMorphism :: Morphism a b a' b' -> TypedDigraph a b -> TypedDigraph a' b'
applyMorphism = undefined
