module Graph.Digraph where

import Data.IntMap

-- Edge idE (idSrc, idTgt) payload
data Edge a = Edge Int (Int, Int) a deriving (Show,Eq,Read)
-- Node idN payload
data Node a = Node Int a deriving (Show,Eq,Read)

data Digraph a b = Digraph (IntMap (Node a)) (IntMap (Edge b))

addNode :: Node a -> Digraph a b -> Maybe (Digraph a b)
addNode n@(Node id a) g@(Digraph nm em)
    | id `member` nm = Nothing
    | otherwise = Just $ Digraph (insert id n nm) em

addEdge :: Edge b -> Digraph a b -> Maybe (Digraph a b)
addEdge = undefined

delNode :: Node a -> Digraph a b -> Maybe (Digraph a b)
delNode = undefined

delEdge :: Edge b -> Digraph a b -> Maybe (Digraph a b)
delEdge = undefined

nodes :: Digraph a b -> [Node a]
nodes = undefined

edges :: Digraph a b -> [Edge b]
edges = undefined

conn :: Edge b -> Digraph a b -> Maybe (Node a, Node a)
conn = undefined
