module Matrix	( mkZeros
				, mkMatrix
				, mGetColumn
				, mGetLine
				, mSetLine
				, mGetCell
				, mSetCell
				, mTranspose
				, mMult
				, Matrix -- I don't like to expose internals, but I may have too.
				, mColumns
				, mLines
				, matrix
				, foldColumns
				, foldCells
				, zipMatrix
				, toList
				, toLatexTabular
				, printLatexTabular
				) where

import Data.List

-- Use mkMatrix instead.
data Matrix a = Matrix	{ matrix :: [[a]]
						, mLines :: Int
						, mColumns :: Int
						} deriving (Show,Eq)

mkZeros :: Int -> Int -> Matrix Int
mkZeros l c = Matrix { matrix = m, mLines = l, mColumns = c }
	where m = take l $ repeat $ take c $ repeat 0

mkMatrix :: [[a]] -> Matrix a
mkMatrix m = if foldl (\b ls -> b && (length ls == prime)) True $ tail m
	then Matrix { matrix = m, mColumns = length $ m !! 0, mLines = length m }
	else error "Line size mismatch"
		where prime = length $ head m

mGetColumn :: Int -> Matrix a -> [a]
mGetColumn c m = map (\ls -> ls !! c) $ matrix m

mGetLine :: Int -> Matrix a -> [a]
mGetLine l m = (matrix m) !! l

mSetLine :: Int -> Matrix a -> [a] -> Matrix a
mSetLine l m vs = if length vs == mColumns m 
	then let (hs, ts) = splitAt l $ matrix m in m { matrix = hs ++ [vs] ++ tail ts }
	else error $ "Line size incorrect: " ++ (show $ length vs) ++ "(expected: " ++ (show $ mColumns m) ++ ")"

mGetCell :: Int -> Int -> Matrix a -> a
mGetCell l c m = ((matrix m) !! l) !! c

mSetCell :: a -> Int -> Int -> Matrix a -> Matrix a
mSetCell v c l m = mSetLine l m newLine{-Cinema-}
	where newLine = let (hs, ts) = splitAt c $ mGetLine l m in hs ++ [v] ++ tail ts

mTranspose :: Matrix a -> Matrix a
mTranspose m = Matrix { matrix = transposed, mColumns = mLines m, mLines = mColumns m }
	where 
		transposed = reverse $ foldl (\cs c -> iterateColumns c:cs) [] $ [0..(mColumns m) - 1]
		iterateColumns c = reverse $ foldl (\l ls -> (ls !! c):l) [] $ matrix m

foldColumns :: (a -> [b] -> a) -> a -> Matrix b -> a
foldColumns f d m = foldl (\v c -> f v (mGetColumn c m)) d [0..(mColumns m) - 1]

foldCells :: (a -> b -> a) -> a -> Matrix b -> a
foldCells f d m = foldl (\a b -> foldl (\c e -> f c ((matrix m !! b) !! e)) a [0..(mColumns m) - 1]) d [0..(mLines m) - 1]

zipMatrix :: Matrix a -> Matrix b -> Matrix (a, b)
zipMatrix a b = if (mColumns a == mColumns b) && (mLines a == mLines b)
	then Matrix { matrix = zipped, mColumns = mColumns a, mLines = mLines a }
	else error "Matrix size mismatch"
		where
			zipped = reverse $ foldl (\ls i -> zip (mGetLine i a) (mGetLine i b):ls) [] [0..(mLines a) - 1]

mMult :: Num a => Matrix a -> Matrix a -> Matrix a
mMult p q = if mColumns p /= mLines q
	then error "Columns and lines mismatch"
	else Matrix { matrix = multiplied, mColumns = mColumns q, mLines = mLines p }
		where
			multiplied = map (\x -> map (\y -> multVec (mGetLine x p) (mGetColumn y q)) [0..n]) [0..n]
			multVec a b = foldl (\v i -> (a !! i) * (b !! i) + v) 0 [0..(mColumns q) - 1]
			n = mColumns q - 1

toList :: Matrix a -> [a]
toList m = let
	ms = matrix m
	cs = [0..(mColumns m) - 1]
	ls = [0..(mLines m) - 1]
	in do
		l <- ls
		c <- cs
		return $ (ms !! l) !! c

toLatexTabular :: Show a => Matrix a -> [String]
toLatexTabular m = 
	header : (map ((++ " \\\\") . intersperse ' ' . intersperse '&' . map (head . show)) $ matrix m) ++ [footer]
	where
		colDef = take (mColumns m) $ repeat 'r'
		header = "\\left[\\begin{tabular}{" ++ colDef ++ "}"
		footer = "\\end{tabular}\\right]"

printLatexTabular :: Show a => Matrix a -> IO ()
printLatexTabular = mapM_ putStrLn . toLatexTabular