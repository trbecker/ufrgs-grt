module Graph.Digraph 
    ( Edge
    , Node
    , Digraph
    , addNode
    , addEdge
    , removeNode
    , removeEdge
    , nodes
    , edges
    ) where

import Data.IntMap

-- Edge idE (idSrc, idTgt) payload
data Edge a = Edge Int (Int, Int) a deriving (Show,Eq,Read)
-- Node idN payload
data Node a = Node Int a deriving (Show,Eq,Read)

data Digraph a b = Digraph (IntMap (Node a)) (IntMap (Edge b)) deriving (Show)
    
empty :: Digraph a b
empty = Digraph (Data.IntMap.empty) (Data.IntMap.empty)

addNode :: (Monad m) => Node a -> Digraph a b -> m (Digraph a b)
addNode n@(Node id _) g@(Digraph nm em) =
    if id `member` nm 
        then fail $ "addNode: node " ++ show id ++ " already in digraph"
        else return $ Digraph (insert id n nm) em

addEdge :: (Monad m) => Edge b -> Digraph a b -> m (Digraph a b)
addEdge e@(Edge id (s, t) _) g@(Digraph nm em)
    | id `member` em = 
        fail $ "addEdge: edge " ++ show id ++ " already in digraph"
    | s `member` nm && t `member` nm =
        return $ Digraph nm (insert id e em)
    | otherwise =
        fail $ "addEdge: edge has nodes not found in digraph"

removeNode :: (Monad m) => Node a -> Digraph a b -> m (Digraph a b)
removeNode n@(Node id _) g@(Digraph nm em) =
    if id `member` nm 
        then return $ Digraph (delete id nm) em
        else fail $ "removeNode: node " ++ show id ++ " not in digraph"

removeEdge :: (Monad m) => Edge b -> Digraph a b -> m (Digraph a b)
removeEdge e@(Edge id _ _) g@(Digraph nm em) =
    if id `member` em 
        then return $ Digraph nm (delete id em)
        else fail $ "removeEdge: edge " ++ show id ++ " not in Digraph"

nodes :: Digraph a b -> [(Node a)]
nodes (Digraph nm _) = Prelude.map snd $ toList nm

edges :: Digraph a b -> [(Edge b)]
edges (Digraph _ em) = Prelude.map snd $ toList em

