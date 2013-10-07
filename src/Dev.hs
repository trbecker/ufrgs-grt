{-# LANGUAGE FlexibleContexts #-}
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

typedIsomorphism :: (Graph g, Typed (NodePayload g), Typed (EdgePayload g)) =>
	(Node (NodePayload g) -> Node (NodePayload g) -> Bool) ->
	(Edge (EdgePayload g) -> Edge (EdgePayload g) -> Bool) -> 
	g -> g -> [Matrix Int]
typedIsomorphism fn fe = isomorphisms
