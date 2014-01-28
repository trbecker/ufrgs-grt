module Graph.Match where

import Graph.Digraph
import Graph.TypedDigraph
import qualified Data.IntMap as IM
import qualified Data.List as L

type Mapping = (Int, Int)

findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Morphism a b]
findMatches l g = undefined
	
{- | given two Graphs 'l' and 'g', an Edge id from Graph 'l' 'eid' and 
   a Morphism 's' (representing the current state), returns a list of valid 
   Morphisms where the given edge and it's source/target nodes are mapped
   to all valid members (i.e. those that satisfy the CSP) from graph 'g'.
-}
matchEdge :: TypedDigraph a b -> TypedDigraph a b -> Int -> Morphism a b -> [Morphism a b]
matchEdge = undefined

{- | and edgeConstraint is a function that receives a Graph 'g' together with
the corresponding edge id 'gid' and tests if 'gid' satisfies the given
condition.  If so, it returns a list of two mappings with the corresponding
src's and target nodes 
-}
type EdgeConstraint a b = Int -> TypedDigraph a b -> Maybe [Mapping]

{- | nodeConstraints are functions that receive a Graph 'g' together with the
   corresponding node id 'gid' and return True if 'gid' satisfies the given
   condition.
-}
type NodeConstraint a b = Int -> TypedDigraph a b -> Maybe [(Int, Int)]

type EdgeCSP a b = [EdgeConstraint a b]

{- | takes an existing list of mapped nodes 'p' (a pair of node id's, the
first corresponding to the 'l' graph and the second to the 'g' one), an Edge Id
'lid' and it's typedDigraph 'l' and returns an EdgeConstraint.
addEdgeConstraint checks if the given Edge has some node as it's source/target
already in 'p'. If so, it returns a constraint that forces any matching edge to
have the corresponding node as it's source/target.  Otherwise it only restricts
the source/target's type.
-}
addEdgeConstraint
	:: [(Int, Int)] 
	-> Int 
	-> TypedDigraph a b
	-> EdgeConstraint a b
addEdgeConstraint p lid l@(TypedDigraph (Digraph lnm lem) _) = let
	lEdge@(Just (Edge _ (lsrc, ltar) _)) = IM.lookup lid lem
	lsrcType = getNodeType lsrc l
	ltarType = getNodeType ltar l
	-- checks if src/tar nodes were already mapped
	matchedSrc = (\(ln, gn) -> ln == lsrc) `L.find` p
	matchedTar = (\(ln, gn) -> ln == lsrc) `L.find` p 
	checkSrc = case matchedSrc of
		Just (ln, gn) -> (\x -> x == gn)
		otherwise	  -> (\x -> True)
	checkTar = case matchedTar of 
		Just (ln, gn) -> (\x -> x == gn)
		otherwise	  -> (\x -> True)
	constraint = (\gid g@(TypedDigraph (Digraph gnm gem) _) -> let
		gEdge@(Just (Edge _ (gsrc, gtar) _)) = IM.lookup gid gem
		gsrcType = getNodeType gsrc g
		gtarType = getNodeType gtar g
		in
			if	(lsrcType == gsrcType
		    	&& (ltarType == gtarType)
		    	&& checkSrc gsrc
		    	&& checkTar gtar)
			then
		    	Just $ (lsrc, gsrc):(ltar, gtar):p
			else
				Nothing)
	in contraint

