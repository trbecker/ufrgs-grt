module Graph.TypedDigraph where

import Control.Monad

import Graph.Digraph

type TGraph = Digraph () ()

type NodeAction a = (Maybe (Node a), Maybe (Node a))
type EdgeAction a = (Maybe (Edge a), Maybe (Edge a))

data Morphism a b = Morphism [NodeAction a] [EdgeAction b]
data TypedDigraph a b = TypedDigraph (Digraph a b) TGraph (Morphism a b)

nodeAction :: (Monad m, Eq a) => NodeAction a -> (Digraph a b -> m (Digraph a b))
nodeAction (Nothing, Just n) = addNode n
nodeAction (Just n, Nothing) = removeNode n 
nodeAction (Just n, Just n') = if n /= n' 
							then const $ fail "Unmatched nodes in keep action"
							else keepNode n
nodeAction (Nothing, Nothing) = return

edgeAction :: (Monad m, Eq b) => EdgeAction b -> (Digraph a b -> m (Digraph a b))
edgeAction (Nothing, Just e) = addEdge e
edgeAction (Just e, Nothing) = removeEdge e
edgeAction (Just e, Just e') = if e /= e'
							then const $ fail "Unmatched edges in keep action"
							else keepEdge e
edgeAction (Nothing, Nothing) = return

actionSet :: (Monad m, Eq a, Eq b) => Morphism a b -> [Digraph a b -> m (Digraph a b)]
actionSet (Morphism na ea) = let
	nodeActions f = map nodeAction . filter f
	edgeActions f = map edgeAction . filter f
	knSet = nodeActions (\(Just s, Just t) -> True) na
	keSet = edgeActions (\(Just s, Just t) -> True) ea
	anSet = nodeActions (\(Nothing, Just t) -> True) na
	aeSet = edgeActions (\(Nothing, Just t) -> True) ea
	dnSet = nodeActions (\(Just s, Nothing) -> True) na
	deSet = edgeActions (\(Just s, Nothing) -> True) ea
	in deSet ++ dnSet ++ anSet ++ aeSet ++ knSet ++ keSet


applyMorphism :: (Monad m, Eq a, Eq b) => Morphism a b -> Digraph a b -> m (Digraph a b)
applyMorphism m g = foldM (\g f -> f g) g $ actionSet m
