module Graph.TypedDigraph where

import Control.Monad

import Graph.Digraph

type TGraph a b = Digraph a b

type TypeInfo a = (Int, a)

data TypedDigraph a b = TypedDigraph (Digraph (TypeInfo a) (TypeInfo b)) (TGraph a b)

getNodeType :: Int -> TypedDigraph a b -> Maybe Int                             
getNodeType id td@(TypedDigraph (Digraph nm em) _) =                            
        let x = IM.lookup id nm                                                 
        in case x of                                                            
                Nothing -> Nothing                                              
                Just (Node _ (tid, _)) -> Just tid
