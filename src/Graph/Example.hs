import qualified Graph.Digraph as D

g = D.empty

n1 = D.Node 1 "um"
n2 = D.Node 2 "dois"
e1 = D.Edge 1 (1, 2) "um -> dois"
e2 = D.Edge 2 (2, 3) "dois -> tres"

g1 = D.addNode n1 g :: Maybe (D.Digraph String String)
g2 = g1 >>= D.addNode n2 :: Maybe (D.Digraph String String)
g3 = g2 >>= D.addEdge e1 :: Maybe (D.Digraph String String)

