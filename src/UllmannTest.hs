module UllmannTest 
	( module Data.Graph
	, module Ullmann
	, module Diagraph
	, alpha
	, beta
	, right
	) where

import Control.Monad

import Ullmann
import Data.Graph
import Diagraph

-- Auxiliar functions
right (Right x) = x
flip3 f a b c = f a c b
app x f = f x

alpha :: Diagraph
alpha = right $ foldM app empty nodeOps >>= flip3 foldM app edgeOps
	where
		nodeOps = [ addNode (Node EmptyNode [] 0)
				  , addNode (Node EmptyNode [] 1)
				  , addNode (Node EmptyNode [] 2)
				  ]
		edgeOps = [ addEdge (Edge EmptyEdge (0, 1) 0)
				  , addEdge (Edge EmptyEdge (1, 2) 1)
				  ]

beta :: Diagraph
beta = right $ foldM app empty nodeOps >>= flip3 foldM app edgeOps
	where
		nodeOps =	[ addNode (Node EmptyNode [] 0)
					, addNode (Node EmptyNode [] 1)
					, addNode (Node EmptyNode [] 2)
					, addNode (Node EmptyNode [] 3)
					, addNode (Node EmptyNode [] 4)
					]
		edgeOps =	[ addEdge (Edge EmptyEdge (0, 1) 0)
					, addEdge (Edge EmptyEdge (1, 2) 2)
					, addEdge (Edge EmptyEdge (1, 3) 3)
					, addEdge (Edge EmptyEdge (3, 4) 4)
					]
