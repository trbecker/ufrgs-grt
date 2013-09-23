module Dev	( module Ullmann
			, module UllmannTest
			) where

import Ullmann
import UllmannTest
import Control.Monad

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

mFoldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
mFoldM _ a [] = return a
mFoldM f a (x:xs) = f a x >>= \fax -> mFoldM f fax xs

iterate :: Matrix Int -> [Matrix Int]
iterate m = mFoldM (flip nextDepth) m [0..(mLines m) - 1]