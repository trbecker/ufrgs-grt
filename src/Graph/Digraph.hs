module Graph.Digraph 
    ( Edge
    , Node
    , Digraph
    , node
    , edge
    , empty
    , addNode
    , addEdge
    , removeNode
    , removeEdge
    , keepNode
    , keepEdge
    , nodes
    , edges
    ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM


-- Edge idE (idSrc, idTgt) payload
data Edge a = Edge Int (Int, Int) a deriving (Show,Eq,Read)
-- Node idN payload
data Node a = Node Int a deriving (Show,Eq,Read)

node = Node

edge = Edge

data Digraph a b = Digraph (IntMap (Node a)) (IntMap (Edge b)) deriving (Show)

empty :: Digraph a b
empty = Digraph (IM.empty) (IM.empty)

addNode :: (Monad m) => Node a -> Digraph a b -> m (Digraph a b)
addNode n@(Node id _) g@(Digraph nm em) =
    if id `IM.member` nm 
        then fail $ "addNode: node " ++ show id ++ " already in digraph"
        else return $ Digraph (IM.insert id n nm) em

addEdge :: (Monad m) => Edge b -> Digraph a b -> m (Digraph a b)
addEdge e@(Edge id (s, t) _) g@(Digraph nm em)
    | id `IM.member` em = 
        fail $ "addEdge: edge " ++ show id ++ " already in digraph"
    | s `IM.member` nm && t `IM.member` nm =
        return $ Digraph nm (IM.insert id e em)
    | otherwise =
        fail $ "addEdge: edge points to nodes not found in digraph"

removeNode :: (Monad m) => Node a -> Digraph a b -> m (Digraph a b)
removeNode n@(Node id _) g@(Digraph nm em)
    | id `IM.notMember` nm =
        fail $ "removeNode: node " ++ show id ++ " not in digraph"
    | IM.fold 
        (\(Edge eid (s, t) _) acc -> acc || s == id || t == id) 
        False em =
        fail $ "removeNode: node " ++ show id ++ " has some edge pointing to it"
    | otherwise =
        return $ Digraph (IM.delete id nm) em

removeEdge :: (Monad m) => Edge b -> Digraph a b -> m (Digraph a b)
removeEdge e@(Edge id _ _) g@(Digraph nm em) =
    if id `IM.member` em 
        then return $ Digraph nm (IM.delete id em)
        else fail $ "removeEdge: edge " ++ show id ++ " not in digraph"

keepNode :: (Monad m) => Node a -> Digraph a b -> m (Digraph a b)
keepNode (Node nid _) g@(Digraph ns es) = if nid `IM.member` ns
							then return g
							else fail $ "keepNode: node " ++ show nid ++ " doesn't exist"

keepEdge :: (Monad m) => Edge b -> Digraph a b -> m (Digraph a b)
keepEdge (Edge eid _ _) g@(Digraph ns es) = if eid `IM.member` es
							then return g
							else fail $ "keepEdge: edge " ++ show eid ++ " doesn't exist"

nodes :: Digraph a b -> [(Node a)]
nodes (Digraph nm _) = map snd $ IM.toList nm

edges :: Digraph a b -> [(Edge b)]
edges (Digraph _ em) = map snd $ IM.toList em

