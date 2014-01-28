module Graph.Match where

import Graph.Digraph
import Graph.TypedDigraph
import qualified Data.IntMap as IM

findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Morphism a b]
findMatches l g = undefined
    
{- | given two Graphs 'l' and 'g', an Edge id from Graph 'l' 'eid' and 
   a Morphism 's' (representing the current state), returns a list of valid 
   Morphisms where the given edge and it's source/target nodes are mapped
   to all valid members (i.e. those that satisfy the CSP) from graph 'g'.
-}
matchEdge :: TypedDigraph a b -> TypedDigraph a b -> Int -> Morphism a b -> [Morphism a b]
matchEdge = undefined

{- | edgeConstraints are functions that receive a Graph 'g' together with the
   corresponding edge id 'gid' and return True if 'gid' satisfies the given
   condition.
-}
type EdgeConstraint = Int -> TypedDigraph a b -> Bool

{- | nodeConstraints are functions that receive a Graph 'g' together with the
   corresponding node id 'gid' and return True if 'gid' satisfies the given
   condition.
-}
type NodeConstraint = Int -> TypedDigraph a b -> Bool

type EdgeCSP = [EdgeConstraint]



{- | takes an existing list of mapped edges 'me' (represented as a tuple (src,
tar)), an Edge Id 'lid' and it's typedDigraph 'l' and returns a tuple where the
first element is the list of mapped edges with the new edge added and the
second is an edgeConstraint.  addEdgeConstraint checks if the given Edge has
some node as it's source/target already in 'me'. If so, it returns a constraint
that forces any matching edge to have the corresponding node as it's
source/target.
-}
addEdgeConstraint :: [(Int, Int)] -- already mapped edges [(src, tar)]
                    -> Int        -- edge id 'lid'
                    -> TypedDigraph a b -- 'l' graph
                    -> ([(Int, Int)], EdgeConstraint)
addEdgeConstraint me lid l@(TypedDigraph (Digraph nm em) _)  = let
    lEdge@(Just (Edge _ (lsrc, ltar) _)) = IM.lookup lid em
    lsrcType = getNodeType lsrc l
    ltarType = getNodeType ltar l
    srcFound = lsrc `elem` me
    tarFound = ltar `elem` me
    checkSrc = if srcFound
        then (\x -> x == lsrc)
        else (\x -> True)
    checkTar = if tarFound
        then (\x -> x == ltar)
        else (\x -> True)
    constraint = (\gid g -> let
        gEdge@(Just (Edge _ (gsrc, gtar) _)) = IM.lookup gid g
        gsrcType = getNodeType gsrc g
        gtarType = getNodeType gtar g
        in ((lsrcType == gsrcType)
            && (ltarType == gtarType)
            && checkSrc
            && checkTar))
    in ((lsrc, ltar):me, constraint)
                
            
sameType :: NodeConstraint
sameType l lid g gid = let
    lType = getNodeType l lid
    gType = getNodeType g gid
    in lType == gType

-- rewrite rule graph match
rewrite :: (Monad m) => Rule a b -> TypedDigraph a b -> Morphism a b -> m TypedDigraph a b
rewrite = undefined
-- fmap (rewrite rule graph) matches
