module Dev	( module Ullmann
			, module UllmannTest
			, module Control.Monad
			) where

import Ullmann
import UllmannTest
import Control.Monad
import Data.List

extractNodeId (x:xs)
	| x == 1 = 0
	| otherwise = 1 + extractNodeId xs

toNodeIdList :: Matrix Int -> [Int]
toNodeIdList m = let 
	ms = matrix m
	in do
		l <- ms
		return $ extractNodeId l

isomorphMap :: Matrix Int -> [(Int, Int)]
isomorphMap m = let
	is = [0..(length $ matrix m) - 1]
	ms = matrix m
	in do
		i <- is
		let l = ms !! i
		return (i, extractNodeId l)

nextDepth :: Int -> Matrix Int -> [Matrix Int]
nextDepth d m = do
	c <- [0..(mColumns m) - 1]
	return $ selectColumn c d m

nextDepthFilter :: (Matrix Int -> Bool) -> Int -> Matrix Int -> [Matrix Int]
nextDepthFilter f d = filter f . nextDepth d

nextDepthL d = concat . map (nextDepth d)

nextDepthLFilter f d = concat . map (nextDepthFilter f d)

iter pruner m = foldM (flip (nextDepthFilter pruner)) m [0..(mLines m) - 1]

bfEnum pruninigCondition finalCondition = filter finalCondition . iter pruninigCondition

enumCandidates :: Graph g => (Matrix Int -> Bool) -> g -> g -> [Matrix Int]
enumCandidates finalCondition alpha = 
	bfEnum surjectiveCondition finalCondition . buildMatrix alpha

enumHomomorphisms :: Graph g => 
								(Matrix Int -> Bool) -> 
								(Matrix Int -> Matrix Int -> Matrix Int -> Bool) ->
								g ->
								g ->
								[Matrix Int]
enumHomomorphisms finalCondition validityCondition alpha beta = let
	ma = adjacencyMatrix alpha
	mb = adjacencyMatrix beta
	in filter (validityCondition ma mb) $ enumCandidates finalCondition alpha beta

checkMapping :: Matrix Int -> Matrix Int -> Matrix Int-> Bool
checkMapping ma mb mk = let 
	mc = mMult mk . mTranspose . mMult mk $ mb
	in not . any (\(a, c) -> c > 0 && not (a == 1)) . toList $ zipMatrix ma mc

surjectiveCondition :: Matrix Int -> Bool
surjectiveCondition = not . any (\x -> not . any (==1) $ x) . matrix

injectiveCondition :: Matrix Int -> Bool
injectiveCondition = not . any (\c -> sum c > 1) . matrix . mTranspose