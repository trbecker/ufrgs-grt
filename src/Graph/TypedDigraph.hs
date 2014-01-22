module Graph.TypedDigraph where

import Control.Monad

import Graph.Digraph

type TGraph = Digraph () () -- Digraph (IntMap (Node ())) (IntMap (Edge ()))
                            -- Node () = Node Int ()
                            -- Edge () = Edge Int (Int, Int) ()

type TypeInfo a = (Int, a)

data TypedDigraph a b = TypedDigraph (Digraph (TypeInfo a) (TypeInfo b)) (TGraph a b)


