{-# LANGUAGE GADTs, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module Data.Graph where

import qualified Data.Map as M
import Matrix

-- Node payload incoming_list, outgoing_list, id
data Node p = Node p [Int] Int deriving (Show)
-- Edge payload (src, dst) id
data Edge p = Edge p (Int, Int) Int deriving (Show)


class Graph g where
	-- Maybe unload the payloads from the typeclass, so they can be defined outside it
	type NodePayload g :: *
	type EdgePayload g :: *
	type Mapping g     :: * -> *

	nodes :: g -> Mapping g (Node (NodePayload g))
	edges :: g -> Mapping g (Edge (EdgePayload g))

	nodeKeys :: g -> [Int]
	edgeKeys :: g -> [Int]

	getNode :: Int -> g -> Maybe (Node (NodePayload g))
	getEdge :: Int -> g -> Maybe (Edge (EdgePayload g))

	{- Returns the old graph in case of an error, or the new one if the node is added successfuly -}
	addNode :: Node (NodePayload g) -> g -> Either (String,g) g -- no way I'm using ErrorT.
	{- Returns the old graph in case of an error, or the new one if the edge is added successfuly -}
	addEdge :: Edge (EdgePayload g) -> g -> Either (String,g) g

	delNode :: Int -> g -> Either (String, g) g
	delEdge :: Int -> g -> Either (String, g) g

	-- Creation, from mappings
	fromMappings :: Mapping g (Node (NodePayload g)) -> Mapping g (Edge (EdgePayload g)) -> g

	union :: g -> g -> g

	-- Filter nodes and edges independently. Default filterGDep (\_ n -> fn n) (\_ e -> fe e)
	filterG :: (Node (NodePayload g) -> Node (NodePayload g)) -> (Edge (EdgePayload g) -> Edge (EdgePayload g)) -> g -> g
	filterG fn fe = filterGDep (\_ n -> fn n) (\_ e -> fe e)
	-- Filter edges; filter functions may depend on the graph.
	-- Filtering must not leave the graph information inconsistent.
	filterGDep :: (g -> Node (NodePayload g) -> Node (NodePayload g)) -> (g -> Edge (EdgePayload g) -> Edge (EdgePayload g)) -> g -> g

--  foldG :: (Node (NodePayload g) -> Node (NodePayload g)) -> (Edge (EdgePayload g) -> Edge (EdgePayload g)) -> g -> g
	foldEdges :: (a -> Edge (EdgePayload g) -> a) -> a -> g -> a

	foldNodes :: (a -> Node (NodePayload g) -> a) -> a -> g -> a


class GraphNode n where
	connectedEdges :: n -> [Int]

-- Some important type classes
class Typed a where
	type Type a :: *
	getType :: a -> Type a

class Weigthed a where
	type Weigth a :: *
	weigth :: a -> Weigth a

class Labeled a where
	label :: a -> String

-- Instances for nodes
instance Typed p => Typed (Node p) where
	type Type (Node p) = Type p
	getType (Node payload _ _) = getType payload

instance Weigthed p => Weigthed (Node p) where
	type Weigth (Node p) = Weigth p
	weigth (Node p _ _) = weigth p

instance Labeled p => Labeled (Node p) where
	label (Node p _ _) = label p

instance GraphNode (Node p) where
	connectedEdges (Node _ es _) = es

-- Instances for edges
instance Typed p => Typed (Edge p) where
	type Type (Edge p) = Type p
	getType (Edge p _ _) = getType p

instance Weigthed p => Weigthed (Edge p) where
	type Weigth (Edge p) = Weigth p
	weigth (Edge p _ _) = weigth p

instance Labeled p => Labeled (Edge p) where
	label (Edge p _ _) = label p

degree :: Node a -> Int
degree (Node _ ls _) = length ls

connectedNodes (Edge _ np _) = np

setUndirected e = let (n1, n2) = connectedNodes e in mSetCell 1 n1 n2 . mSetCell 1 n2 n1

adjacencyMatrix :: Graph g => g -> Matrix Int
adjacencyMatrix = adjMatrix (flip setUndirected)

adjMatrix :: Graph g => (Matrix Int -> Edge (EdgePayload g) -> Matrix Int) -> g -> Matrix Int
adjMatrix f g = let
	ks = nodeKeys g
	n = length ks
	in foldEdges f (mkZerosWithLabels n n ks ks) g

setDirected e = let (s, t) = connectedNodes e in mSetCell 1 t s

adjacencyMatrixDirected :: Graph g => g -> Matrix Int
adjacencyMatrixDirected = adjMatrix (flip setDirected)



nodeId (Node _ _ i) = i
