module Graph.Match
	( EdgeConstraint (..)
	, conditionList
	, applyCond
	, testFunc
	)
	where

import Graph.Digraph
import Graph.TypedDigraph
import qualified Data.IntMap as IM
import qualified Data.List as L


findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Morphism a b]
findMatches l g = undefined
	
{- | given two Graphs 'l' and 'g', an Edge id from Graph 'l' 'eid' and 
   a Morphism 's' (representing the current state), returns a list of valid 
   Morphisms where the given edge and it's source/target nodes are mapped
   to all valid members (i.e. those that satisfy the CSP) from graph 'g'.
-}
matchEdge :: TypedDigraph a b -> TypedDigraph a b -> Int -> Morphism a b -> [Morphism a b]
matchEdge = undefined

type Mapping = [(Int, Int)]

{- | and edgeConstraint is a function that receives a Graph 'g' together with
the corresponding edge id 'gid' and tests if 'gid' satisfies the given
condition.  If so, it returns a list of two mappings with the corresponding
src's and target nodes 
-}
type EdgeConstraint a b = Int -> TypedDigraph a b -> Maybe Mapping

{- | nodeConstraints are functions that receive a Graph 'g' together with the
   corresponding node id 'gid' and return True if 'gid' satisfies the given
   condition.
-}
type NodeConstraint a b = Int -> TypedDigraph a b -> Maybe [(Int, Int)]

type EdgeCSP a b = [EdgeConstraint a b]

{- Conditions are used to construct Constraints. A Condition consists of a
function that, given a Mapping 'p' and an Edge 'le' together with it's
TypedDigraph 'l', returns another function that consums an edge 'ge' and it's
TypedDigraph 'g' and returns True if 'ge' satisfies the condition.
-}
type Condition a b =
	Mapping 
	-> TypedDigraph a b
	-> Edge b 
	-> TypedDigraph a b
	-> Edge b
	-> Bool

srcTypeCond :: Condition a b
srcTypeCond p l le g ge =
	ltype == gtype
	where
		ltype = srcType le l
		gtype = srcType ge g

tarTypeCond :: Condition a b
tarTypeCond p l le g ge =
	ltype == gtype
	where
		ltype = tarType le l
		gtype = tarType ge g


srcIDCond :: Condition a b
srcIDCond p l le@(Edge _ (lsrc, _) _) g ge@(Edge _ (gsrc, _) _) =
	let matched = (\(ln, gn) -> ln == lsrc) `L.find` p
	in case matched of	
		Just (ln, gn) -> gsrc == gn
		Nothing -> True

tarIDCond :: Condition a b
tarIDCond p l le@(Edge _ (_, ltar) _) g ge@(Edge _ (_, gtar) _) =
	let matched = (\(ln, gn) -> ln == ltar) `L.find` p
	in case matched of	
		Just (ln, gn) -> gtar == gn
		Nothing -> True

conditionList = [srcTypeCond, tarTypeCond, srcIDCond, tarIDCond]

{- returns the 'p' Mapping with the new source/target pairs added if all 
conditions in 'cl' get satisfied -}
satisfiesCond
	:: [Condition a b]
	-> Mapping
	-> TypedDigraph a b
	-> Edge b
	-> TypedDigraph a b
	-> Edge b
	-> Maybe Mapping
satisfiesCond cl p l le g ge =
	if foldr (\c acc -> (c p l le g ge) || acc) False cl 
	then Just $ (target le, target ge) : (source le, source ge) : p
	else Nothing

applyCond
	:: [Condition a b]
	-> Mapping
	-> TypedDigraph a b
	-> Edge b
	-> TypedDigraph a b
	-> [Mapping]
applyCond cl p l le g@(TypedDigraph (Digraph _ gem) _) =
	let candidates = IM.mapMaybe (\ge -> satisfiesCond cl p l le g ge) gem
	in IM.foldr (\c acc -> c : acc) [] candidates 

testFunc :: Int -> TypedDigraph a b -> TypedDigraph a b -> Maybe [Mapping]
testFunc id l@(TypedDigraph (Digraph _ em) _) g =
	let le = IM.lookup id em
	in
		case le of
			Just e -> Just $ applyCond conditionList [] l e g
			Nothing -> Nothing

	


{- | takes an existing list of mapped nodes 'p' (a pair of node id's, the
first corresponding to the 'l' graph and the second to the 'g' one), an Edge Id
'lid' and it's typedDigraph 'l' and returns an EdgeConstraint.
addEdgeConstraint checks if the given Edge has some node as it's source/target
already in 'p'. If so, it returns a constraint that forces any matching edge to
have the corresponding node as it's source/target.  Otherwise it only restricts
the source/target's type.
-}
{-
addEdgeConstraint
	:: Mapping
	-> [Condition a b]
	-> Int 
	-> TypedDigraph a b
	-> EdgeConstraint a b
addEdgeConstraint p cl lid l@(TypedDigraph (Digraph lnm lem) _) = let
	lEdge@(Just (Edge _ (lsrc, ltar) _)) = IM.lookup lid lem
	lsrcType = srcType lEdge l
	ltarType = tarType lEdge l
	-- checks if src/tar nodes were already mapped
	matchedSrc = (\(ln, gn) -> ln == lsrc) `L.find` p
	matchedTar = (\(ln, gn) -> ln == ltar) `L.find` p 
	checkSrc = case matchedSrc of
		Just (ln, gn) -> (\x -> x == gn) :: Int -> Bool
		otherwise	  -> (\x -> True)
	checkTar = case matchedTar of 
		Just (ln, gn) -> (\x -> x == gn) :: Int -> Bool
		otherwise	  -> (\x -> True)
	constraint = (\gid g@(TypedDigraph (Digraph gnm gem) _) -> let
		gEdge@(Just (Edge _ (gsrc, gtar) _)) = IM.lookup gid gem
		gsrcType = srcType gEdge g
		gtarType = tarType gEdge g
		in
			if	(lsrcType == gsrcType
		    	&& (ltarType == gtarType)
		    	&& checkSrc gsrc
		    	&& checkTar gtar)
			then
		    	Just $ (lsrc, gsrc):(ltar, gtar):p
			else
				Nothing)
	in constraint
-}

{- given an EdgeConstraint 'ec' and a TypedDigraph 'tg', returns a list of 
possible new mappings that satisfy constraint
-}
{-
applyConstraint
	:: 
	EdgeConstraint a b
	-> TypedDigraph a b
	-> [Mapping]
applyConstraint ec tg@(TypedDigraph (Digraph _ em) _) =
	let mappings = IM.mapMaybe (\e@(Edge eid _ _) -> ec eid tg) em 
	in IM.foldr (\m acc -> m:acc) [] mappings 	
-}

