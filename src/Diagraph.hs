{-# LANGUAGE TypeFamilies, GADTs #-}
module Diagraph where

import Control.Monad
import Control.Monad.IO.Class
import Data.Graph
import qualified Data.Map as M

data MNodePayload where
	EmptyNode :: MNodePayload

data MEdgePayload where
	EmptyEdge :: MEdgePayload 

data Diagraph = Diagraph (M.Map Int (Node MNodePayload)) (M.Map Int (Edge MEdgePayload)) deriving (Show)

instance Show MNodePayload where
	show EmptyNode = "(unlabeled)"

instance Show MEdgePayload where
	show EmptyEdge = "(unlabeled)"

instance Typed MNodePayload where
	type Type MNodePayload = Int
	getType _ = 0

instance Typed MEdgePayload where
	type Type MEdgePayload = Int
	getType _ = 0


instance Graph Diagraph where
	type NodePayload Diagraph = MNodePayload
	type EdgePayload Diagraph = MEdgePayload
	type Mapping Diagraph     = (M.Map Int)

	nodes (Diagraph nm _ ) = nm
	edges (Diagraph _  em) = em

	nodeKeys g = M.keys $ nodes g
	edgeKeys g = M.keys $ edges g

	getNode k dg = M.lookup k $ nodes dg
	getEdge k dg = M.lookup k $ edges dg

	addNode = dgAddNode
	addEdge = dgAddEdge

	delNode = dgDelNode
	delEdge = dgDelEdge

	fromMappings = undefined

	union (Diagraph nm1 em1) (Diagraph nm2 em2) = Diagraph (M.union nm1 nm2) (M.union em1 em2)

	filterGDep = undefined

	foldEdges f v g = M.foldl f v $ edges g
	foldNodes f v g = M.foldl f v $ nodes g

-- replaces
-- nodes -> countSet nodes
-- edges -> countSet edges
countSet :: Num a => Diagraph -> (Diagraph -> M.Map Int t) -> a
countSet dg fn = fromIntegral $ M.size $ fn dg

-- replaces
-- maxNode -> maxIdx nodes
-- maxEdge -> maxIdx edges
maxIdx :: Num a => (Diagraph -> M.Map Int t) -> Diagraph -> a
maxIdx fn dg = let
	mmap = fn dg
	in if M.null mmap
		then 0
		else fromIntegral . fst $ M.findMax mmap

-- Constructors
empty :: Diagraph
empty = Diagraph M.empty M.empty

{-
	Tries to add a node to the graph, returning the updated graph in the rigth or 
	an error description and the old graph in the left.
-}
dgAddNode :: Node MNodePayload -> Diagraph -> Either (String, Diagraph) Diagraph
dgAddNode node@(Node p [] nodeId) graph@(Diagraph nm em) = let 
	newGraph = Right $ Diagraph (M.insert nodeId node nm) em
	in maybe newGraph (\_ -> Left ("Repeated node id " ++ show nodeId, graph)) (M.lookup nodeId nm)

dgAddNode (Node p xs nodeId) g = Left ("Node must have an empty list of edges when inserted", g)
-- I swear, if I can find a damn graph monad, I'll marry it

{-
	Tries to add a new edge between two existing nodes to the graph, returning the new graph on
	the right, or an error description and the old graph in the left.
-}
dgAddEdge :: Edge MEdgePayload -> Diagraph -> Either (String, Diagraph) Diagraph
dgAddEdge edge@(Edge p (src, dst) edgeId) graph@(Diagraph nm em) = 
	let
		-- adds an incident edge to a node.
		addIncidentEdge (Node p eIds nodeId) = Just $ Node p (edgeId:eIds) nodeId
		-- updates the node map with the new incidnet edges
		newNodeMap = M.update addIncidentEdge src $ M.update addIncidentEdge dst nm
		-- adds the new edge to the graph
		newGraph = Right $ Diagraph newNodeMap (M.insert edgeId edge em)
		-- verifies if the node n is in the graph.
		nodeCheck n = maybe (Left ("Missing node id " ++ show n, graph)) (\_ -> newGraph) (M.lookup n nm)
		-- checks if the edgeId already exists.
		checkEdge = maybe newGraph (\_ -> Left ("Repeated edge id " ++ show edgeId, graph)) (M.lookup edgeId em)
		-- Inside the Either monad, take a look at http://stackoverflow.com/questions/5112212/is-there-no-standard-either-a-monad-instance
		in checkEdge >> nodeCheck src >> nodeCheck dst

dgDelEdge :: Int -> Diagraph -> Either (String, Diagraph) Diagraph
dgDelEdge idx graph@(Diagraph nm em) = let
	edge = getEdge idx graph
	rmAdjacentEdge (Node p es nodeId) = Just $ Node p (filter (/= idx) es) nodeId
	newNodes = maybe nm (\(Edge p (src,dst) edgeId) -> M.update rmAdjacentEdge src $ M.update rmAdjacentEdge dst nm) edge
	in maybe (Left ("No edge with id " ++ show idx, graph)) (\_ -> Right $ Diagraph newNodes (M.delete idx em)) edge

dgDelNode :: Int -> Diagraph -> Either (String, Diagraph) Diagraph
dgDelNode idx graph@(Diagraph nm em) = let
	node = getNode idx graph
	mEdges = maybe (Left ("No node with id " ++ show idx, graph)) (\n -> Right $ connectedEdges n) node
	rmNode (Diagraph nm em) = maybe (Left ("No node with id " ++ show idx, graph)) (\_ -> Right $ Diagraph (M.delete idx nm) em) node
	in mEdges >>= foldM (flip dgDelEdge) graph >>= rmNode

-- DPO: can't delete a node that is connected
dgDelNodeDPO :: Int -> Diagraph -> Either (String, Diagraph) Diagraph
dgDelNodeDPO idx graph@(Diagraph nm em) = let
	node = getNode idx graph
	canDelete = (node >>= return . connectedEdges) == Just []
	in if canDelete
		then maybe (Left ("No node with id " ++ show idx, graph)) (\_ -> Right $ Diagraph (M.delete idx nm) em) node
		else Left ("Node " ++ show idx ++ " is connected to others", graph)

-- FIX: The edges are kept from the old graph.
inducedSubgraph :: [Int] -> Diagraph -> Diagraph
inducedSubgraph ns g = let
	nds = M.filterWithKey (\k _ -> k `elem` ns) (nodes g)
	eds = M.filter (\(Edge _ (a, b) _) -> a `elem` ns && b `elem` ns) (edges g)
	in Diagraph nds eds
