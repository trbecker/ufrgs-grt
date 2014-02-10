module Graph.Digraph 
    ( Edge (..)
    , Node (..)
    , Digraph
    , Morphism (..)
    , Action (..)
    , ActionList (..)
    , empty
    , addNode
    , addEdge
    , removeNode
    , removeEdge
    , keepNode
    , keepEdge
    , nodes
    , edges
    , nodeMap
    , edgeMap
    , applyMorphism
    , applyActions
    , actionSet
    , nodeMorphisms
    , edgeMorphisms
    , nodeId
    , edgeId
    ) where

import Control.Monad

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM


-- Edge idE (idSrc, idTgt) payload
data Edge a = Edge Int (Int, Int) a deriving (Show,Eq,Read)
edgeId (Edge id _ _) = id

-- Node idN payload
data Node a = Node Int a deriving (Show,Eq,Read)
nodeId (Node id _) = id

data Digraph a b = Digraph (IntMap (Node a)) (IntMap (Edge b)) deriving (Show,Eq)

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
keepNode (Node nid _) g@(Digraph ns es) = 
    if nid `IM.member` ns
        then return g
        else fail $ "keepNode: node " ++ show nid ++ " doesn't exist"

keepEdge :: (Monad m) => Edge b -> Digraph a b -> m (Digraph a b)
keepEdge (Edge eid _ _) g@(Digraph ns es) = 
    if eid `IM.member` es
        then return g
        else fail $ "keepEdge: edge " ++ show eid ++ " doesn't exist"

nodes :: Digraph a b -> [(Node a)]
nodes (Digraph nm _) = map snd $ IM.toList nm

edges :: Digraph a b -> [(Edge b)]
edges (Digraph _ em) = map snd $ IM.toList em

nodeMap (Digraph ns _ ) = ns
edgeMap (Digraph _  es) = es

type Action c a     = (Maybe (c a), Maybe (c a))
type ActionList c a = [Action c a]

data Morphism a b = Morphism (ActionList Node a) (ActionList Edge b) deriving (Show)

nodeMorphisms (Morphism nm _ ) = nm
edgeMorphisms (Morphism _  em) = em

nodeAction :: (Monad m, Eq a) => Action Node a -> Digraph a b -> m (Digraph a b)
nodeAction (Nothing, Just n) = addNode n
nodeAction (Just n, Nothing) = removeNode n 
nodeAction (Just n, Just n') = if n /= n' 
							then const $ fail "Node transformation is unhandled"
							else keepNode n
nodeAction (Nothing, Nothing) = return

edgeAction :: (Monad m, Eq b) => Action Edge b -> Digraph a b -> m (Digraph a b)
edgeAction (Nothing, Just e) = addEdge e
edgeAction (Just e, Nothing) = removeEdge e
edgeAction (Just e, Just e') = if e /= e'
							then const $ fail "Edge transformation is unhandled"
							else keepEdge e
edgeAction (Nothing, Nothing) = return

addAction (Nothing, Just t) = True
addAction _ = False

removeAction (Just s, Nothing) = True
removeAction _ = False

keepAction (Just s, Just t) = True
keepAction _ = False

--actionSet :: (Monad m, Eq a, Eq b) => Morphism a b -> [Digraph a b -> m (Digraph a b)]
actionSet (Morphism na ea) = let
	nodeActions f = map nodeAction . filter f
	edgeActions f = map edgeAction . filter f
	knSet = nodeActions keepAction na
	keSet = edgeActions keepAction ea
	anSet = nodeActions addAction na
	aeSet = edgeActions addAction ea
	dnSet = nodeActions removeAction na
	deSet = edgeActions removeAction ea
	in deSet ++ dnSet ++ anSet ++ aeSet ++ knSet ++ keSet

applyActions :: Monad m => a -> [a -> m a] -> m a
applyActions = foldM (\g f -> f g)

applyMorphism :: (Monad m, Eq a, Eq b) => Morphism a b -> Digraph a b -> m (Digraph a b)
applyMorphism m g = applyActions g $ actionSet m
