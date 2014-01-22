module Graph.TypedDigraph where

import Control.Monad

import Graph.Digraph

type TGraph a b = Digraph a b

type TypeInfo a = (Int, a)

data TypedDigraph a b = TypedDigraph (Digraph (TypeInfo a) (TypeInfo b)) (TGraph a b)
