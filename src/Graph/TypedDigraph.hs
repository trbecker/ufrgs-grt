module Graph.TypedDigraph where

import Control.Monad

import Graph.Digraph

type TGraph a b = Digraph a b

type TypeInfo a = (Int, a)

data TypedDigraph a b = TypedDigraph (Digraph (TypeInfo a) (TypeInfo b)) (TGraph a b)
	deriving (Show,Eq)

type TypedMorphism a b = Morphism (TypeInfo a) (TypeInfo b)

applyTypedMorphism :: (Monad m, Eq a, Eq b) => 
	TypedMorphism a b -> 
	TypedDigraph a b -> 
	m (TypedDigraph a b)
applyTypedMorphism m (TypedDigraph g t) = liftM (flip TypedDigraph t) $ applyActions g $ actionSet m

