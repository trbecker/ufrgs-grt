module Ullmann	( module Data.Graph
				, module Matrix
				, buildMatrix
				, selectColumn
				, iterateColumns
				, bruteForceIsomorphism
				) where

import Data.Maybe
import Data.Graph
import Matrix

ullmann = undefined

buildMatrix :: Graph g => g -> g -> Matrix Int
buildMatrix match target = foldNodes (\mat m -> foldNodes (compareAndSet m) mat target) (mkZeros lenMatch lenTarget) match
	where
		lenMatch = length $ nodeKeys match
		lenTarget = length $ nodeKeys target
		compareAndSet m matrix t = if degree m <= degree t then mSetCell 1 (nodeId t) (nodeId m) matrix else matrix

selectColumn :: Int -> Int -> Matrix Int -> Matrix Int
selectColumn col depth m = mSetLine depth m $ zeros col ++ [mGetCell depth col m] ++ zeros ((mColumns m) - col - 1)

iterateColumns :: Int -> Matrix Int -> [Matrix Int]
iterateColumns depth m = filterValid $ map (\c -> selectColumn c depth m) [0..(mColumns m ) - 1]
	where
		isValid m = not $ foldl (\b v -> b && (v == 0)) True $ (matrix m) !! depth
		filterValid = filter isValid

-- I have a problem naming things. My six cats are called Cat, Cat, Cat, Cat, Cat and Cat; 
-- Just for fun, I should call one of them Hound.
bruteForceIsomorphism :: Graph g => g -> g -> [Matrix Int]
bruteForceIsomorphism match target = filterRepeated $ filterNothing $ map (\m -> if compare a (mMult m $ mTranspose $ mMult m $ adjacencyMatrix target)
		then Just m
		else Nothing) $ iterate
	where
		mat = buildMatrix match target
		a = adjacencyMatrix match
		compare a b = foldCells (\v (a, b) -> v && (a /= 1 || b == 1)) True $ zipMatrix a b
		iterate = clearRepeatedColumns $ iterateDepth (mLines mat - 1) [mat]
		countNonzeros vec = foldl (\t v -> if v >= 1 then t + 1 else t) 0 vec
		clearRepeatedColumns mats = filter (\m -> foldColumns (\v cs -> v && (countNonzeros cs <= 1)) True m) mats
		iterateDepth _ [] = []
		iterateDepth 0 ms = asdf 0 ms
		iterateDepth d ms = iterateDepth (d-1) $ asdf d ms
		asdf d ms = foldl (\ms m -> iterateColumns d m ++ ms) [] ms
		filterNothing ms = foldl (\ms x -> if x == Nothing then ms else (fromJust x):ms) [] ms
		filterRepeated ms = foldl (\ms m -> if not $ m `inList` ms then m:ms else ms) [] ms

inList :: Eq a => a -> [a] -> Bool
v `inList` xs = any (\x -> x == v) xs

zeros n = take n $ repeat 0
