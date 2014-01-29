module Graph.Match
	(
	conditionList
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

{- | A Condition consists of a function that, given a Mapping 'p', two
TypedDigraphs 'l', 'g' and two Edges 'le', 'ge', checks if 'ge' satisfies it
-}
type Condition a b =
	Mapping 
	-> TypedDigraph a b
	-> Edge b 
	-> TypedDigraph a b
	-> Edge b
	-> Bool

{- | checks if 'le' and 'ge' have source nodes from same type -}
srcTypeCond :: Condition a b
srcTypeCond p l le g ge =
	ltype == gtype
	where
		ltype = srcType le l
		gtype = srcType ge g

{- | checks if 'le' and 'ge' have target nodes from same type -}
tarTypeCond :: Condition a b
tarTypeCond p l le g ge =
	ltype == gtype
	where
		ltype = tarType le l
		gtype = tarType ge g

{- | figures out if 'le's source already occurs in 'p'. If that's the case,
srcIDCond checks if 'ge's source is the same node to which 'le's source got
mapped.  If so, 'ge' is a matching Edge. If 'le's source doesn't occur in 'p',
any 'ge' will satisfy this condition
-}
srcIDCond :: Condition a b
srcIDCond p l le@(Edge _ (lsrc, _) _) g ge@(Edge _ (gsrc, _) _) =
	let matched = (\(ln, gn) -> ln == lsrc) `L.find` p
	in case matched of	
		Just (ln, gn) -> gsrc == gn
		Nothing -> True

{- | figures out if 'le's target already occurs in 'p'. If that's the case,
tarIDCond checks if 'ge's target is the same node to which 'le's target got
mapped.  If so, 'ge' is a matching Edge. If 'le's target doesn't occur in 'p',
any 'ge' will satisfy this condition
-}
tarIDCond :: Condition a b
tarIDCond p l le@(Edge _ (_, ltar) _) g ge@(Edge _ (_, gtar) _) =
	let matched = (\(ln, gn) -> ln == ltar) `L.find` p
	in case matched of	
		Just (ln, gn) -> gtar == gn
		Nothing -> True

conditionList = [srcTypeCond, tarTypeCond, srcIDCond, tarIDCond]

{- | if all conditions in 'cl' get satisfied by the given edge 'ge', returns the
'p' Mapping with the new source/target pairs added. 
-}
satisfiesCond
	:: [Condition a b]
	-> Mapping
	-> TypedDigraph a b
	-> Edge b
	-> TypedDigraph a b
	-> Edge b
	-> Maybe Mapping
satisfiesCond cl p l le g ge =
	if foldr (\c acc -> (c p l le g ge) && acc) True cl 
	then Just $ (target le, target ge) : (source le, source ge) : p
	else Nothing

{- | given a Condition list, a Mapping 'p', two Graphs 'l', 'g' and an Edge
'le', searches for all edges from graph 'g' that satisfies the conditions in
this context.  Returns a list of Mappings, each with the new possibility added
-}
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

{- | written for test purposes in Example.hs.  -}
testFunc :: Int -> TypedDigraph a b -> TypedDigraph a b -> Maybe [Mapping]
testFunc id l@(TypedDigraph (Digraph _ em) _) g =
	let le = IM.lookup id em
	in
		case le of
			Just e -> Just $ applyCond conditionList [] l e g
			Nothing -> Nothing
