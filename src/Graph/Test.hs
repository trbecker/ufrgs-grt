module Graph.Test where

import Control.Monad

import Data.Maybe

import Graph.Digraph
import Graph.TypedDigraph


-- Prelude's Either monad is broken, since fail only accepts Strings as arguments
data Error r = Err String | OK r deriving (Eq)

instance Monad Error where
	return x = OK x
	(Err y) >>= _ = Err y
	(OK x) >>= f = (f x)
	fail = Err

instance (Show r) => Show (Error r) where
	show (OK x) = show x
	show (Err msg) = unwords ["err:", msg]

alpha = fromJust $ foldM (\g f -> f g) empty	[ addNode $ Node 1 ()
												, addNode $ Node 2 ()
												, addNode $ Node 3 ()
												, addNode $ Node 0 ()
												, addEdge $ Edge 1 (1, 2) ()
												, addEdge $ Edge 2 (2, 3) ()
												]

morphism1 = Morphism [(Nothing, Just $ Node 4 ())] [(Nothing, Just $ Edge 3 (3, 4) ())]
morphism2 = Morphism [] [(Just $ Edge 3 (3,4) (), Nothing)]
morphism3 = Morphism [(Just $ Node 1 (), Nothing)] []
morphism4 = Morphism [(Just $ Node 0 (), Nothing), (Just $ Node 0 (), Just $ Node 0 ())] []

alpha' = fromJust $ applyMorphism morphism1 alpha

successRemoveEdge :: Maybe (Digraph () ())
successRemoveEdge = applyMorphism morphism2 alpha'

failRemoveNode :: Maybe (Digraph () ())
failRemoveNode = applyMorphism morphism3 alpha'

failKeepNode :: Error (Digraph () ())
failKeepNode = applyMorphism morphism4 alpha
