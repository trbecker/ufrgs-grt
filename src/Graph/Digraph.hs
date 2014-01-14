module Graph.Digraph 
    ( Edge
    , Node
    , Digraph
    , addNode
    , addEdge
    , delNode
    , delEdge
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

addNode :: Node a -> Digraph a b -> Digraph a b
addNode n@(Node id _) g@(Digraph nm em) =
    if id `member` nm 
        then g
        else Digraph (insert id n nm) em

addEdge :: Edge b -> Digraph a b -> Digraph a b
addEdge e@(Edge id _ _) g@(Digraph nm em) =
    if id `member` em 
        then g
        else Digraph nm (insert id e em)

delNode :: Node a -> Digraph a b -> Digraph a b
delNode n@(Node id _) g@(Digraph nm em) =
    if id `member` nm 
        then Digraph (delete id nm) em
        else g

delEdge :: Edge b -> Digraph a b -> Digraph a b
delEdge e@(Edge id _ _) g@(Digraph nm em) =
    if id `member` em 
        then Digraph nm (delete id em)
        else g

nodes :: Digraph a b -> [(Node a)]
nodes (Digraph nm _) = Prelude.map snd $ toList nm

edges :: Digraph a b -> [(Edge b)]
edges (Digraph _ em) = Prelude.map snd $ toList em

