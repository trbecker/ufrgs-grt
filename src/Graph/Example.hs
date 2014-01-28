import Control.Monad
import qualified Graph.Digraph as D

l = D.empty
g = D.empty

ln = [D.Node 1 "one", 
	  D.Node 2 "two",
	  D.Node 3 "three"]
le = [D.Edge 4 (1, 2) "one -> two",
	  D.Edge 5 (2, 3) "two -> three"]

l1 = foldM (\d n -> D.addNode n d)  l ln :: Maybe (D.Digraph String String)
l2 = l1 >>=
	(\l -> foldM (\d e -> D.addEdge e d) l le:: Maybe (D.Digraph String String))

	
gn = [D.Node 10 "a",
	  D.Node 11 "b",
	  D.Node 12 "c",
	  D.Node 13 "d"]
ge = [D.Edge 15 (10, 11) "a -> b",
	  D.Edge 16 (10, 12) "a -> c",
	  D.Edge 17 (12, 13) "c -> d"]
g1 = foldM (\d n -> D.addNode n d)  l ln :: Maybe (D.Digraph String String)
g2 = g1 >>=
	(\l -> foldM (\d e -> D.addEdge e d) l le:: Maybe (D.Digraph String String))
