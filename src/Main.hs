{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

import Data.IORef
import Data.Maybe
import Data.Either
import Data.List
import Data.Ord
import Data.Fixed
import qualified Data.Map      as M
import qualified Data.Set      as S
import qualified Control.Monad as C
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Layout
import Text.XML.HXT.Core


{- |
   'MapGraph' is a graph representation usable for graph editing. It is not the most efficient in terms of memory. However, 
   there is a balance between the amount of time required to access\/update\/augment the structure.

   It consists of two maps:
     
     * NodeMap: maps each node to its incident edges and respective information about the type of edge.
     
     * EdgeMap: maps each edge to its incident source and target nodes.

   For generality, ids of nodes and edges are hardcoded as Integers. Additions and deletions of edges and nodes update both mappings.
-}
data MapGraph = MapGraph NodeMap EdgeMap deriving (Eq,Ord,Show,Read)


-- | 'IncType' describes the kind of incidence an edge may have upon a node: output ('Tgt'), input ('Src') or bidirectional ('Both')
data IncType = Src | Tgt | Both deriving (Eq,Ord,Show,Read)

type NodeId = Integer

type EdgeId = Integer

type NodeMap  = M.Map NodeId [(EdgeId,IncType)]    
{- ^ 'NodeMap' maps each node to its incident edges (with the incidence type) -}

type EdgeMap  = M.Map EdgeId (NodeId,NodeId,Bool)  
{- ^ 'EdgeMap' maps each edge to its source and target nodes, and a boolean that is True if the edge is directed, or False otherwise -}
                                                   

--- Global access to the structure

-- | 'nodeMap' 'g' returns the map of nodes of a MapGraph 'g'
nodeMap :: MapGraph -> NodeMap
nodeMap (MapGraph mn me) = mn


-- | 'edgeMap' returns the map of edges of a MapGraph
edgeMap :: MapGraph -> EdgeMap
edgeMap (MapGraph mn me) = me


-- | 'nodes' returns the list of node identifiers of the graph
nodes :: MapGraph -> [NodeId]
nodes g = M.keys $ nodeMap g


-- | 'edges' returns the list of edge identifiers of the graph
edges :: MapGraph -> [EdgeId]
edges g = M.keys $ edgeMap g


-- | 'numNodes' returns the number of nodes of the graph
numNodes :: Num a => MapGraph -> a
numNodes g = fromIntegral $ M.size $ nodeMap g


-- | 'numEdges' returns the number of edges of the graph
numEdges :: Num a => MapGraph -> a
numEdges g = fromIntegral $ M.size $ edgeMap g



maxNode :: Num a => MapGraph -> a
maxNode (MapGraph mn me) =
  if M.null mn
    then 0
    else fromIntegral x
         where (x,_) = M.findMax mn

maxEdge :: Num a => MapGraph -> a
maxEdge (MapGraph mn me) =
  if M.null me
    then 0
    else fromIntegral x
         where (x,_) = M.findMax me


--- Constructors / modifiers

empty :: MapGraph
empty = MapGraph M.empty M.empty


addNode :: NodeId -> MapGraph -> MapGraph
addNode n (MapGraph mn me) = MapGraph (M.insert n [] mn) me


addEdge :: EdgeId -> NodeId -> NodeId -> Bool -> MapGraph -> MapGraph
addEdge e s t b (MapGraph mn me) =
     MapGraph
       (if s==t
          then M.insertWith (++) s [(e,Both)] mn
          else if b
                  then M.insertWith (++) s [(e,Src)] (M.insertWith (++) t [(e,Tgt)] mn)
                  else M.insertWith (++) s [(e,Both)] (M.insertWith (++) t [(e,Both)] mn))
       (M.insert e (s,t,b) me)


delEdge :: EdgeId -> MapGraph -> MapGraph
delEdge e g =
   case M.lookup e (edgeMap g) of
    Nothing    -> g
    Just (s,t,_) ->
      MapGraph
        (M.adjust (\l-> filter (\p->(fst p)/=e) l)  s
           (M.adjust (\l-> filter (\p->(fst p)/=e) l) t
             (nodeMap g)))
        (M.delete e (edgeMap g))


delNode :: NodeId -> MapGraph -> MapGraph
delNode n g =
  let es = incidentEdges n g
      g' = foldr delEdge g es
  in MapGraph (M.delete n $ nodeMap g') (edgeMap g')


inducedSubgraph :: [NodeId] -> MapGraph -> MapGraph
inducedSubgraph ns g =
  let nds = M.filterWithKey (\k _ -> k `elem` ns) (nodeMap g)
      eds = M.filter (\(a,b,_)->a `elem` ns && b `elem` ns) (edgeMap g)
  in MapGraph nds eds



mergeNodes v v' g =
  let minV = min v v'
      maxV = max v v'
  in mapGr (\n -> if n==maxV then minV else n) id g





-- Applies a translation function to nodes and edges simultaneously.

mapGr :: (NodeId -> NodeId) -> (EdgeId -> EdgeId) -> MapGraph -> MapGraph
mapGr fn fe g =
  let
    nds =  M.foldrWithKey (\k a b -> M.insert (fn k) (map (\(e,t)->(fe e,t)) a) b)  M.empty  (nodeMap g)
    eds =  M.foldrWithKey (\k a b -> M.insert (fe k) ((\(x,y,z)->(fn x,fn y,z)) a) b) M.empty (edgeMap g)
  in MapGraph nds eds



-- Operations between graphs


-- converts and association list to the respective function
assocToFunc :: (Eq a) => [(a,a)] -> a -> a
assocToFunc l = foldr (\(x,y) m -> (\z -> if z == x then y else (m z)) ) id l


-- rearrange the keys (nodes and edges) within a graph, to put them in the range 1..n
compactKeys :: MapGraph -> MapGraph
compactKeys g =
  let fn = assocToFunc $ zip (M.keys $ nodeMap g) [1..]
      fe = assocToFunc $ zip (M.keys $ edgeMap g) [1..]
  in mapGr fn fe g


-- graph union, as in set union
grUnion :: MapGraph -> MapGraph -> MapGraph
grUnion g1 g2 =
  MapGraph (M.union (nodeMap g1) (nodeMap g2))
           (M.union (edgeMap g1) (edgeMap g2))


-- graph disjoint union
grDisjointUnion :: MapGraph -> MapGraph -> MapGraph
grDisjointUnion g1 g2 =
  let
    g1'  = compactKeys g1
    maxN = maxNode g1
    maxE = maxEdge g1
    g2'  = mapGr (+maxN) (+maxE) $ compactKeys g2
  in grUnion g1' g2'



--- Conversion from/to lists

fromList :: ([NodeId],[(EdgeId,NodeId,NodeId,Bool)]) -> MapGraph
fromList (nl,el) =
  let
    nullgraph = (foldr addNode empty nl)
    mapgraph  = (foldr (\(a,x,y,b)->addEdge a x y b) nullgraph el)
  in mapgraph


toList :: MapGraph -> ([NodeId],[(EdgeId,NodeId,NodeId,Bool)])
toList (MapGraph mn me) = ((M.keys mn), map (\(e,(x,y,b))->(e,x,y,b)) (M.assocs me))


--- Access nodes from edges

src :: EdgeId -> MapGraph -> Maybe NodeId
src e g = do (s,t,_) <- M.lookup e (edgeMap g)
             return s

tgt :: EdgeId -> MapGraph -> Maybe NodeId
tgt e g = do (s,t,_) <- M.lookup e (edgeMap g)
             return t

endPoints :: EdgeId -> MapGraph -> Maybe (NodeId,NodeId)
endPoints e g = do
                (a,b,_) <- M.lookup e (edgeMap g)
                return (a,b)

incidentNodes :: EdgeId -> MapGraph -> [NodeId]
incidentNodes e g =  (maybeToList $ src e g) ++ (maybeToList $ tgt e g)





--- Access edges from nodes

outEdges :: NodeId -> MapGraph -> [EdgeId]
outEdges n g = case M.lookup n (nodeMap g) of
                 Nothing -> []
                 Just el -> map fst $ filter (\(x,y)->y==Src||y==Both) el

inEdges :: NodeId -> MapGraph -> [EdgeId]
inEdges n g = case M.lookup n (nodeMap g) of
                 Nothing -> []
                 Just el -> map fst $ filter (\(x,y)->y==Tgt||y==Both) el

incidentEdges :: NodeId -> MapGraph -> [EdgeId]
incidentEdges n g = case M.lookup n (nodeMap g) of
                      Nothing -> []
                      Just el -> map fst el


edgesFromTo :: NodeId -> NodeId -> MapGraph -> [EdgeId]
edgesFromTo n1 n2 g = M.foldrWithKey 
                          (\k (s,t,dir) b -> 
                             if (dir==False) && ((s==n1&&t==n2)||(s==n2&&t==n1))
                               then (k:b)
                               else if (dir==True) && (s==n1) && (t==n2)
                                      then (k:b) 
                                      else b)     
                          []
                          (edgeMap g)


edgesBetween :: NodeId -> NodeId -> MapGraph -> [EdgeId]
edgesBetween n1 n2 g = M.foldrWithKey 
                          (\k (s,t,dir) b -> 
                             if (dir==False) && ((s==n1&&t==n2)||(s==n2&&t==n1))
                               then (k:b)
                               else if (dir==True) && (s==n1) && (t==n2)
                                      then (k:b) 
                                      else if (dir==True) && (s==n2) && (t==n1)     
                                             then (k:b)
                                             else b)
                          []
                          (edgeMap g)



-- Access nodes from nodes

outNodes :: NodeId -> MapGraph -> [NodeId]
outNodes n g = do e  <- outEdges n g
                  n' <- delete n $ incidentNodes e g
                  return n'
                           

inNodes :: NodeId -> MapGraph -> [NodeId]
inNodes n g = do e  <- inEdges n g
                 n' <- maybeToList $ src e g
                 return n'



--- Degree counting

inDeg :: Integral a => NodeId -> MapGraph -> a
inDeg n g  = genericLength $ inEdges n g

outDeg :: Integral a => NodeId -> MapGraph -> a
outDeg n g = genericLength $ outEdges n g

deg :: Integral a => NodeId -> MapGraph -> a
deg n g = (inDeg n g) + (outDeg n g)

minDeg :: Integral a => MapGraph -> a
minDeg g = aux (nodes g)
           where
           aux []    = 0
           aux [n]   = deg n g
           aux (n:t) = min (deg n g) (aux t)

maxDeg :: Integral a => MapGraph -> a
maxDeg g = aux (nodes g)
           where
           aux []    = 0
           aux [n]   = deg n g
           aux (n:t) = max (deg n g) (aux t)

regular :: MapGraph -> Bool
regular g = (minDeg g == maxDeg g )



--- Internal relations of a graph


adjacencyMatrix :: MapGraph -> [[String]]
adjacencyMatrix g = 
  let 
    nds = sort $ nodes g
    tbl = map (\a -> (map (\n -> if n `elem` (outNodes a g) then "1" else "0") nds)) nds
    nds' = map show nds
  in ("\\" : nds') : (zipWith (++) (map (:[]) nds') tbl)





-- Known families of graphs

grNull n = fromList ([1..n],[])

grComplete n =
  let nds = [1..n]
      genEds [] = []
      genEds (h:t) = (map (\x->(h,x)) t) ++ (genEds t)
      eds = map (\(c,(a,b)) -> (c,a,b,False)) (zip [1..] $ genEds nds)
  in
  fromList (nds,eds)

grCycle n =
  let nds = [1..n]
      genEds [] = []
      genEds [x] = [(x,1)]
      genEds (h1:h2:t) = (h1,h2) : (genEds $ h2:t)
      eds = map (\(c,(a,b)) -> (c,a,b,False)) $ zip [1..] $ genEds nds
  in
  fromList (nds,eds)

grPath n =
  let nds = [1..n]
      genEds [] = []
      genEds [x] = []
      genEds (h1:h2:t) = (h1,h2) : (genEds $ h2:t)
      eds = map (\(c,(a,b)) -> (c,a,b,False)) $ zip [1..] $ genEds nds
  in
  fromList (nds,eds)

grGrid m n =
  if m<=0 || n<=0
     then empty
     else let nds = [1..m*n]
              genEds []     = []
              genEds (x:t)  = (if x `mod` n /= 0 then [(x,x+1)] else [])
                              ++
                              (if x <= (m-1)*n then [(x,x+n)] else [])
                              ++ genEds t
              eds = map (\(c,(a,b)) -> (c,a,b,False)) $ zip [1..] $ genEds nds
          in  fromList (nds,eds)

grStar n =
  let nds = [1..n]
      eds = map (\a->(a-1,1,a,False)) [2..n]
  in fromList (nds,eds)



grGenPetersen n k =
  if k >= n
     then empty
     else let
            nds =  [1..n]
            nds' = [1..2*n]
            genEds1 x = [(x,if x/=n then x+1 else 1),(x,x+n)]
            genEds2 x = [(x, if (x+k) > n then (x+k)-n else (x+k))]
            preEds =  (concatMap genEds1 nds) ++
                      (map (\(a,b) -> (a+n,b+n)) $ concatMap genEds2 nds)
            eds = map  (\(c,(a,b)) -> (c,a,b,False)) $ zip [1..] $ preEds
           in fromList (nds',eds)






-----------------------------------------------------------------------


--- Graph layout definitions

data NodeShape  = GLCircle Double
                | GLSquare Double
                | GLRectangle Double Double
                  deriving (Eq,Ord,Read,Show)

data NodeLayout = NodeLayout
                  {
                   nodePos     :: (Double,Double),
                   nodeShape   :: NodeShape,
                   nodeColor   :: (Double,Double,Double),
                   nodeLabel   :: String
                  } deriving (Eq,Ord,Read,Show)

data EdgeLayout = EdgeLayout
  {
    edgePos   :: (Double,Double),
    edgeColor :: (Double,Double,Double),
    edgeLabel :: String
  } deriving (Eq,Ord,Read,Show)


type GraphLayout = (M.Map NodeId NodeLayout, M.Map EdgeId EdgeLayout)




--- Default layout defs

liftNL f = (\(NodeLayout{nodePos=p,nodeShape=s,nodeColor=c,nodeLabel=l}) ->
              NodeLayout {nodePos=(f p),nodeShape=s,nodeColor=c,nodeLabel=l})

liftEL f = (\(EdgeLayout{edgePos=p,edgeColor=c,edgeLabel=l}) ->
               EdgeLayout {edgePos=(f p),edgeColor=c,edgeLabel=l})


wrapDefNodeLayout (x,y) =
  NodeLayout { nodePos = (x,y),
               nodeShape = GLRectangle 15 10,
               nodeColor = (0,0.75,0.75),
               nodeLabel = "" }

wrapDefEdgeLayout (x,y) =
  EdgeLayout { edgePos = (x,y),
               edgeColor = (0,0,1),
               edgeLabel = "" }


getNodePos n nl = do nodeL <- M.lookup n nl
                     return $ nodePos nodeL

getNodeShape n nl = do nodeL <- M.lookup n nl
                       return $ nodeShape nodeL

getNodeColor n nl = do nodeL <- M.lookup n nl
                       return $ nodeColor nodeL

getNodeLabel n nl = do nodeL <- M.lookup n nl
                       return $ nodeLabel nodeL


setNodeLabel n lab dims nl =
 let
   lay = fromMaybe (wrapDefNodeLayout (0,0)) (M.lookup n nl)
   (pw,ph) = dims
   shape = if null lab
             then case nodeShape lay of
               GLCircle _ -> GLCircle $ 5
               GLSquare _ -> GLSquare $ 10
               GLRectangle _ _ -> GLRectangle 15 10
             else case nodeShape lay of
               GLCircle _      -> GLCircle $ 1 + (distance (0,0) (pw/2,ph/2))
               GLSquare _      -> GLSquare $ 2 + (max pw ph)
               GLRectangle _ _ -> GLRectangle (2+pw) (2+ph)
 in M.insert n (lay {nodeLabel = lab, nodeShape = shape}) nl



  


setEdgeLabel e lab  el =
 let
   lay = fromMaybe (wrapDefEdgeLayout (0,0)) (M.lookup e el)
 in M.insert e (lay {edgeLabel = lab}) el




getEdgePos e el = do edgeL <- M.lookup e el
                     return $ edgePos edgeL

getEdgeColor e el = do edgeL <- M.lookup e el
                       return $ edgeColor edgeL

getEdgeLabel e el = do edgeL <- M.lookup e el
                       return $ edgeLabel edgeL



--- Graph  +  Layout  =  DiaGraph

type DiaGraph = (MapGraph,GraphLayout)




--- DiaGraph operations

diagrDelEdge e (g,(nl,el)) =
  (delEdge e g, (nl, M.delete e el))


diagrDelNode n (g,(nl,el)) =
    let nds = nodes g
    in
  (delNode n g,
   (M.delete n nl,
    M.filterWithKey
      (\e _ ->
          fromMaybe False $ do
            (a,b,_) <- M.lookup e (edgeMap g)
            return $ a `elem` nds && b `elem` nds)
      el))



diagrDisjointUnion :: DiaGraph -> DiaGraph -> DiaGraph
diagrDisjointUnion dg1@(g1,(nl1,el1)) dg2@(g2,(nl2,el2)) =
  let
    maxN = maxNode g1
    maxE = maxEdge g1
    dg2'  = mapDiagr (+maxN) (+maxE) $ diagrCompactKeys dg2
  in diagrUnion dg1 dg2'

diagrCompactKeys :: DiaGraph -> DiaGraph
diagrCompactKeys (g,(nl,el)) =
  let fn = assocToFunc $ zip (M.keys $ nodeMap g) [1..]
      fe = assocToFunc $ zip (M.keys $ edgeMap g) [1..]
  in mapDiagr fn fe (g,(nl,el))

diagrUnion :: DiaGraph -> DiaGraph -> DiaGraph
diagrUnion (g1,(nl1,el1)) (g2,(nl2,el2)) =
  (MapGraph (M.union (nodeMap g1) (nodeMap g2))
            (M.union (edgeMap g1) (edgeMap g2)),
   (M.union nl1 nl2, M.union el1 el2))

mapDiagr :: (NodeId -> NodeId) -> (EdgeId -> EdgeId) -> DiaGraph -> DiaGraph
mapDiagr fn fe (g,(nl,el)) =
  let
    nds =  M.foldrWithKey (\k a b -> M.insert (fn k) (map (\(e,t)->(fe e,t)) a) b)  M.empty  (nodeMap g)
    eds =  M.foldrWithKey (\k a b -> M.insert (fe k) ((\(x,y,z)->(fn x,fn y,z)) a) b) M.empty (edgeMap g)
    nl' =  M.foldrWithKey (\k a m -> M.insert (fn k) a m) M.empty nl
    el' =  M.foldrWithKey (\k a m -> M.insert (fe k) a m) M.empty el
  in (MapGraph nds eds, (nl',el'))


inducedSubDiagraph :: [NodeId] -> DiaGraph -> DiaGraph
inducedSubDiagraph ns (g,(nl,el)) =
  let g' = inducedSubgraph ns g
      nl' = M.filterWithKey (\k _ -> k `elem` ns) nl
      eds = edges g'
      el' = M.filterWithKey (\k _ -> k `elem` eds) el
  in (g',(nl',el'))


mergeNodesDiagraph v v' (g,(nl,el)) =
  let g' = mergeNodes v v' g
      maxV = max v v'
      minV = min v v'
      newpoint = fromMaybe (0,0) $ do
                     p1 <- getNodePos v  nl
                     p2 <- getNodePos v' nl
                     return $ midpoint p1 p2
      nl' =  if maxV /= minV
               then M.delete maxV $ M.adjust (liftNL $ const newpoint) minV nl
               else nl
  in (g',(nl',el))




--- Layout algorithms


layoutBBox :: GraphLayout -> (Double,Double,Double,Double)
layoutBBox (nl,el) =
   if M.null nl
     then (0,0,0,0)
     else let f = (\k (x,y,v,w) -> let (a,b) = nodePos k in
                                    (min a x, min b y, max a v, max b w))
              (_,p) = M.findMin nl
              (a,b) = nodePos p

          in (M.fold f (a,b,a,b) nl)




createDefLayout :: MapGraph -> NodeLayout -> EdgeLayout -> DrawingArea -> IO DiaGraph
createDefLayout g defNL defEL da = do 
     let ns = nodes g
     let es = edges g 
     nodeAssocs <- mapM (\n -> do newNL <- updNodeLabel defNL (calcLabel (nodeLabel defNL) n) da
                                  return (n,newNL)) 
                        ns
     let edgeAssocs =  map (\e -> (e, defEL {edgeLabel = calcLabel (edgeLabel defEL) e})) es
     return $ (g,(M.fromList nodeAssocs,M.fromList edgeAssocs))







extendDefLayout :: MapGraph -> (M.Map NodeId (Double,Double)) -> (M.Map EdgeId (Double,Double)) -> NodeLayout -> EdgeLayout -> DrawingArea -> IO DiaGraph
extendDefLayout g nodeCoord edgeCoord defNL defEL  da = do
     let ns = nodes g
     let es = edges g 
     nodeAssocs <- mapM (\n -> do newNL <- updNodeLabel defNL (calcLabel (nodeLabel defNL) n) da
                                  case M.lookup n nodeCoord of
                                    Nothing    -> return (n,newNL)
                                    Just (x,y) -> return (n,newNL{nodePos=(x,y)}))  
                        ns
     let edgeAssocs =  map (\e -> case M.lookup e edgeCoord of
                                    Nothing   -> (e, defEL {edgeLabel = calcLabel (edgeLabel defEL) e})
                                    Just(x,y) -> (e, defEL {edgeLabel = calcLabel (edgeLabel defEL) e, edgePos=(x,y)} )) 
                           es
     return $ (g, (M.fromList nodeAssocs, M.fromList edgeAssocs))
           






circLayout :: DiaGraph -> DiaGraph
circLayout ((MapGraph mn me),(nl,el))  =
  let nds = M.keys mn
      k   = genericLength $ nds
      ang = 2*pi / k
      nl' = M.mapWithKey 
             (\i nodeL -> 
                 let coords = pointAt (3*pi/2+(fromIntegral i-1)*ang) 
                              (min 200 $ max (2*pi*k) 100) 
                              (320,240)
                 in nodeL {nodePos=coords})
             nl
  in ((MapGraph mn me),(nl',el))    
             
                  
  --     f = M.fromList (zip nds points):: M.Map NodeId (Double,Double)
  -- in ((MapGraph mn me),
  --      (M.unionWith (\a b -> a {nodePos=b}) nl nCoords, el)
  --    )
      
  

    --   return $ ((MapGraph mn me),
    --   el = M.fromList $
    --          map
    --            (\(e,(n1,n2,_))->
    --                if n1==n2 -- test if it is a loop
    --                  then case
    --                         do
    --                         (x',y') <- getNodePos n1 nl
    --                         let ang = angle (320,240)  (x',y')
    --                         return $ (ang,20)
    --                       of Nothing    -> (e, defEL {edgePos=(0,20)})
    --                          Just (a,b) -> (e, defEL {edgePos=(a,b)})
    --                  else (e, defEL)) 
    --            eds
    -- in return (MapGraph mn me, (nl,el))




doubleCircLayout :: MapGraph -> DiaGraph
doubleCircLayout (MapGraph mn me)  =
  let nds = M.keys mn
      k   = genericLength $ nds
      len1  = fromIntegral $ floor $ k / 2
      len2  = k - len1
      l1  = take (round len1) nds
      l2  = drop (round len1) nds
      ang1 = 2*pi / len1
      ang2 = 2*pi / len2
      points1 = map (\i->pointAt (3*pi/2+i*ang1) (min 350 $ (10*k) ) (320,240)) [0 .. len1-1]
      points2 = map (\i->pointAt (3*pi/2+i*ang2) (min 250 $ (5*k) ) (320,240)) [0 .. len2-1]

      nl = M.fromList $ zip nds $ map wrapDefNodeLayout (points1 ++ points2)
      eds = M.assocs me
      el = M.fromList $
             map
               (\(e,(n1,n2,_))->
                   if n1==n2 -- test if it is a loop
                     then case
                            do
                            (x',y') <- getNodePos n1 nl
                            let ang = angle (320,240)  (x',y')
                            return $ (ang,20)
                          of Nothing    -> (e,wrapDefEdgeLayout (0,20))
                             Just (a,b) -> (e,wrapDefEdgeLayout (a,b))
                     else (e,wrapDefEdgeLayout (0,0)))
               eds
   in (MapGraph mn me, (nl,el))





gridLayout :: MapGraph -> DiaGraph
gridLayout (MapGraph mn me)  =
  let nds = M.keys mn
      k   = genericLength $ nds
      col = [1..ceiling(sqrt k)]
      points = concatMap (\n->map ((,) n) (map fromIntegral col)) [1..]
      nl = M.fromList $ zip nds $ map wrapDefNodeLayout $ map (\(a,b)->(60*b,60*a)) points
      eds = M.keys me
      edsl = M.assocs me
      el = M.fromList $  zip eds $
                             map
                               (\(a,(x,y,_))->if x==y
                                                then wrapDefEdgeLayout (5*pi/3,20)
                                                else wrapDefEdgeLayout (0,0))
                               edsl
   in (MapGraph mn me, (nl,el))



pathLayout :: MapGraph -> DiaGraph
pathLayout (MapGraph mn me)  =
  let nds = M.keys mn
      k   = genericLength $ nds
      points =  map (\a-> (100+40*a,240)) [1..k]
      nl = M.fromList $ zip nds $ map wrapDefNodeLayout points
      eds = M.keys me
      edsl = M.assocs me
      el = M.fromList $  zip eds $
                             map
                               (\(a,(x,y,_))->if x==y
                                                then wrapDefEdgeLayout (5*pi/3,20)
                                                else wrapDefEdgeLayout (0,0))
                               edsl
   in (MapGraph mn me, (nl,el))



straightenNodes :: GraphLayout -> GraphLayout
straightenNodes (nl,el) =
  let
    nl' = M.map (liftNL (\(a,b) -> (fromIntegral $ 10 * (round $ a/10),
                                    fromIntegral $ 10 * (round $ b/10))))
                nl
  in (nl',el)


updNodeLabel :: NodeLayout -> String -> DrawingArea -> IO NodeLayout
updNodeLabel layout label da = do
    let lay = layout {nodeLabel = label}
    (pw,ph) <- do ctx <- widgetGetPangoContext da
                  pL  <- layoutText ctx $ label
                  (_,PangoRectangle px py pw ph) <- layoutGetExtents pL
                  return $ (pw,ph)
    let shape = if nodeLabel lay == ""
                  then case nodeShape lay of
                         GLCircle _      -> GLCircle $ 5
                         GLSquare _      -> GLSquare $ 10
                         GLRectangle _ _ -> GLRectangle 15 10
                  else case nodeShape lay of
                         GLCircle _      -> GLCircle $ 1 + (distance (0,0) (pw/2,ph/2))
                         GLSquare _      -> GLSquare $ 2 + (max pw ph)
                         GLRectangle _ _ -> GLRectangle (2+pw) (2+ph)
    return $ lay {nodeShape = shape}                 



names :: [String]
names = tail $ gen [[]]
        where  gen x = x ++ gen [ c:s | c <- ['a'..'z'], s<- x ]


calcLabel :: String -> Integer -> String
calcLabel str n =
  if n <= 0
     then ""
     else case str of
           "Number" -> show n
           "Chars"  -> genericIndex names (n-1)
           _        -> ""
    




--- Functions for calculating coordinates


addPoint  (a,b) (c,d) = (a+c,b+d)

diffPoint (a,b) (c,d) = (a-c,b-d)

multPoint (a,b) (c,d) = (a*c,b*d)

distance (a,b) (c,d) = sqrt( (a-c)**2 + (b-d)**2)

midpoint (a,b) (c,d) = ((a+c)/2,(b+d)/2)

angle (a,b) (c,d) =
  case (dx `compare` 0,dy `compare` 0) of
       (LT,LT) -> pi + atan(dy/dx)
       (LT,EQ) -> pi
       (LT,GT) -> pi - atan(-dy/dx)
       (EQ,LT) -> 3*pi/2
       (EQ,EQ) -> 0
       (EQ,GT) -> pi/2
       (GT,LT) -> 2*pi - atan(-dy/dx)
       (GT,EQ) -> 0
       (GT,GT) -> atan(dy/dx)
   where  dy = d-b
          dx = c-a

pointAt :: Double -> Double -> (Double,Double) -> (Double,Double)
pointAt ang dist (x,y) = (x + dist*cos(ang), y + dist*sin(ang))

toPolarFrom :: (Double,Double) -> (Double,Double) -> (Double,Double)
toPolarFrom  (xRef,yRef) (x,y) =
  let ang  = angle (xRef,yRef) (x,y)
      dist = distance  (xRef,yRef) (x,y)
  in (ang,dist)

isPointWithin ((x1,y1),(x2,y2)) (x,y) =
  x <= (max x1 x2)  &&  x >= (min x1 x2)  &&
  y <= (max y1 y2)  &&  y >= (min y1 y2)

quadrant ang =
  let
     a = ang `mod'` pi
     b = if a < 0 then 2*pi + a else a
     c = if (abs a) <= pi/2 then 1 else -1
     d = if b < pi then 1 else -1
  in (c,d)





--- test

genPlaces n = ((-1)^n * pi/2, (fromIntegral $ n`div`2) * 10)



--layoutEdges e (g,(nl,el)) = do (p1,p2) <- endPoints e g



--- Functions to calculate new coordinates for new edges


newEdgePos nid nid' (g,l) =
  fromMaybe (0,0) $
      do p1 <- M.lookup nid  $ fst l
         p2 <- M.lookup nid' $ fst l
         let k  = genericLength $ edgesFromTo nid nid' g
             k' = genericLength $ edgesFromTo nid' nid g
             b  = if k'==0 then 0 else 1
         return $ (-pi/2,20*(k+b))


newLoopPos nid (g,l) =
  fromMaybe (0,0) $
      do p1 <- M.lookup nid  $ fst l
         let k  = (+) 1 $ genericLength $ edgesFromTo nid nid g
         return $ (4*pi/3,20*k)






---------------------------------------------------------------------------
         
         
         
         --------------------------- LATEX --------------------------



diagrToTikz :: DiaGraph -> String
diagrToTikz (g,(nl,el)) =
  let
    header = "%\\usepackage{tikz}\n\\scalebox{0.5}{\n\\begin{tikzpicture}\n"
    opts    = "[every node/.style={draw,thick,circle,fill=gray!60}]\n"
    footer = "\\end{tikzpicture}\n}\n"
    nodeCoords v = let
                      c = fromMaybe (0,0) (getNodePos v nl)
                   in "\\node (n" ++ (show v) ++ ") at (" ++
                        (show $ fst c/20) ++ "," ++ (show $ (-1) * (snd c)/20) ++ ") {};\n" -- ++ (show v) ++ "};\n"
    edgeConnect e = let
                      c = fromMaybe (0,0) (do (a,b,_) <- M.lookup e $ edgeMap g
                                              return (a,b))
                    in "\\draw[thick] (n" ++ (show $ fst c) ++ ") --  (n" ++
                       (show $ snd c) ++ ");\n"
    nds =  concatMap nodeCoords  (nodes g)
    eds =  concatMap edgeConnect (edges g)
  in header ++ opts ++ nds ++ eds ++ footer




--------------------------- XML --------------------------

atTag tag = deep (isElem >>> hasName tag)

maybeGetAttr a = (atTag "attr" >>>
                   hasAttrValue "name" (==a) >>>
                   atTag "string" >>>
                   getChildren >>>
                   getText >>^ Just)
                 `orElse`
                 (constA Nothing)

getNodes = atTag "node" >>>
  proc n -> do
    nid <- getAttrValue "id" -< n
    lay <- maybeGetAttr "layout" -< n
    returnA -< Left (nid,lay)

getEdges = atTag "edge" >>>
  proc e -> do
    from <- getAttrValue "from" -< e
    to   <- getAttrValue "to"   -< e
    lab  <- maybeGetAttr "label" -< e
    lay  <- maybeGetAttr "layout" -< e
    returnA -< Right (from,to,lab,lay)

getGraph = atTag "graph" >>> getNodes <+> getEdges



processGraph xg =
  let conv :: [Either (String,Maybe String) (String,String,Maybe String,Maybe String)]
              -> ([String],[(String,String)])
              -> ([String],[(String,String)])
      conv [] (n,e) = (n,e)
      conv ((Left (s,lay)):t) (n,e) = conv t (s:n,e)
      conv ((Right (n1,n2,lab,lay)):t) (n,e) = conv t (n,(n1,n2):e)

      toF :: (Eq a,Num b) => [(a,b)] -> a -> b
      toF assoc = foldr (\(x,y) m -> (\z -> if z == x then y else (m z)) ) (const 0) assoc

      (nds,eds) = conv xg ([],[])
      f         = toF $ zip nds [1..]
      l         = genericLength nds

      n' = [1..l]
      e'  = map (\(a,b)->(f a, f b)) eds
      e'' = map (\((a,b),c)->(c,a,b,False))  $ zip e' [1..]

  in fromList (n',e'')










--- NEXT STEPS:
--- implement export/import in GXL
--- fix layouters, build GUI for them

-------------------------------------------------------------------------------------------------
---------------------------------------- HELPERS  -----------------------------------------------
-------------------------------------------------------------------------------------------------

-- Infinite list of names to be used in labels



xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)


-------------------------------------------------------------------------------------------------
------------------------------ MENUBAR DEFINITION -----------------------------------------------
-------------------------------------------------------------------------------------------------


buildMaybeMenubar = do 
  
        -- Actions for Menu
        fma  <- actionNew "FMA" "File" Nothing Nothing
        newa <- actionNew "NEWA" "New Graph"     (Just "Just a Stub") (Just stockNew)
        opna <- actionNew "OPNA" "Open Graph"    (Just "Just a Stub") (Just stockOpen)
        sava <- actionNew "SAVA" "Save Graph"    (Just "Just a Stub") (Just stockSave)
        exia <- actionNew "EXIA" "Exit"          (Just "Just a Stub") (Just stockQuit)
        hma  <- actionNew "HMA" "Help" Nothing Nothing
        hlpa <- actionNew "HLPA" "Help"  (Just "Just a Stub") (Just stockHelp)
        agr <- actionGroupNew "AGR"
        mapM_ (actionGroupAddAction agr) [fma, hma]
        mapM_ (\ act -> actionGroupAddActionWithAccel agr act Nothing) [newa,opna,sava,hlpa]
        actionGroupAddActionWithAccel agr exia (Just "<Control>e")

        -- Menu 
        ui <- uiManagerNew
        uiManagerAddUiFromString   ui uiDecl
        uiManagerInsertActionGroup ui agr 0
        maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
        return maybeMenubar

  where uiDecl=  "<ui>\
\           <menubar>\
\            <menu action=\"FMA\">\
\              <menuitem action=\"NEWA\" />\
\              <menuitem action=\"OPNA\" />\
\              <menuitem action=\"SAVA\" />\
\              <menuitem action=\"EXIA\" />\
\            </menu>\
\            <menu action=\"HMA\">\
\              <menuitem action=\"HLPA\" />\
\            </menu>\
\           </menubar>\
\        </ui>"         



-------------------------------------------------------------------------------------------------
---------------------- PANEL DEFINING DEFAULT APPEARENCE FOR NEW NODES AND EDGES ----------------
-------------------------------------------------------------------------------------------------




buildOptPane ::  IORef DiaGraph -> IORef NodeLayout -> IORef EdgeLayout -> IORef Bool -> IO VBox
buildOptPane dgraph defNodeLayout defEdgeLayout defEdgeDir = do
        
        -- Create Vertical Box with Options
        optPane <- vBoxNew False 0  
   
        -- Label for node label options
        l1 <- labelNew Nothing
        labelSetMarkup  l1 "<b>Node label</b>"         
        labelSetJustify l1 JustifyLeft
        boxPackStart optPane l1 PackNatural 5
        ra1 <- radioButtonNewWithLabel "Empty"
        ra2 <- radioButtonNewWithLabelFromWidget ra1 "Number"
        ra3 <- radioButtonNewWithLabelFromWidget ra2 "Chars"
        boxPackStart optPane ra1 PackNatural 0
        boxPackStart optPane ra2 PackNatural 0
        boxPackStart optPane ra3 PackNatural 0
        ra1 `on` toggled $ do nl <- readIORef defNodeLayout
                              writeIORef defNodeLayout $ nl {nodeLabel=""}     
        ra2 `on` toggled $ do nl <- readIORef defNodeLayout
                              (g,l) <- readIORef dgraph
                              let n = maxNode g
                              writeIORef defNodeLayout $ nl {nodeLabel="Number"}     
        ra3 `on` toggled $ do nl <- readIORef defNodeLayout
                              writeIORef defNodeLayout $ nl {nodeLabel="Chars"}     
         -- Label for node color options
        l2 <- labelNew Nothing
        labelSetMarkup l2 "<b>Node color</b>"         
        boxPackStart optPane l2 PackNatural 5
        cb <- colorButtonNewWithColor $ Color 55000 55000 55000
        boxPackStart optPane cb PackNatural 0
        onColorSet cb $ do nl <- readIORef defNodeLayout
                           (Color r g b) <- colorButtonGetColor cb
                           let (r',g',b') = (fromIntegral r, fromIntegral g, fromIntegral b)
                           writeIORef defNodeLayout $ nl {nodeColor=(r'/65535,g'/65535,b'/65535)} 
         
        -- Label for node shape options
        l3 <- labelNew Nothing
        labelSetMarkup l3 "<b>Node shape</b>"         
        boxPackStart optPane l3 PackNatural 5
        rb1 <- radioButtonNewWithLabel "Circle"
        rb2 <- radioButtonNewWithLabelFromWidget rb1 "Square"
        rb3 <- radioButtonNewWithLabelFromWidget rb2 "Rectangle"
        boxPackStart optPane rb1 PackNatural 0
        boxPackStart optPane rb2 PackNatural 0
        boxPackStart optPane rb3 PackNatural 0
        rb1 `on` toggled $ do nl <- readIORef defNodeLayout
                              writeIORef defNodeLayout $ nl {nodeShape=GLCircle 5}     
        rb2 `on` toggled $ do nl <- readIORef defNodeLayout
                              writeIORef defNodeLayout $ nl {nodeShape=GLSquare 10}     
        rb3 `on` toggled $ do nl <- readIORef defNodeLayout
                              writeIORef defNodeLayout $ nl {nodeShape=GLRectangle 15 10}     
         -- Label for edge label options
        l4 <- labelNew Nothing
        labelSetMarkup l4 "<b>Edge label</b>"         
        boxPackStart optPane l4 PackNatural 5
        rc1 <- radioButtonNewWithLabel "Empty"
        rc2 <- radioButtonNewWithLabelFromWidget rc1 "Number"
        rc3 <- radioButtonNewWithLabelFromWidget rc2 "Chars"
        boxPackStart optPane rc1 PackNatural 0
        boxPackStart optPane rc2 PackNatural 0
        boxPackStart optPane rc3 PackNatural 0
        rc1 `on` toggled $ do nl <- readIORef defEdgeLayout
                              writeIORef defEdgeLayout $ nl {edgeLabel=""}     
        rc2 `on` toggled $ do nl <- readIORef defEdgeLayout
                              writeIORef defEdgeLayout $ nl {edgeLabel="Number"}     
        rc3 `on` toggled $ do nl <- readIORef defEdgeLayout
                              writeIORef defEdgeLayout $ nl {edgeLabel="Chars"}     
         -- Label for edge color options
        l6 <- labelNew Nothing
        labelSetMarkup l6 "<b>Edge color</b>"         
        boxPackStart optPane l6 PackNatural 5
        cbe <- colorButtonNewWithColor $ Color 0 0 65535
        boxPackStart optPane cbe PackNatural 0
        onColorSet cbe $ do el <- readIORef defEdgeLayout
                            (Color r g b) <- colorButtonGetColor cbe
                            let (r',g',b') = (fromIntegral r, fromIntegral g, fromIntegral b)
                            writeIORef defEdgeLayout $ el {edgeColor=(r'/65535,g'/65535,b'/65535)} 
        -- Label for edge direction options
        l5 <- labelNew Nothing
        labelSetMarkup l5 "<b>Edge direction</b>"         
        boxPackStart optPane l5 PackNatural 5
        rd1 <- radioButtonNewWithLabel "Undirected"
        rd2 <- radioButtonNewWithLabelFromWidget rd1 "Directed"
        boxPackStart optPane rd1 PackNatural 0
        boxPackStart optPane rd2 PackNatural 0
        rd1 `on` toggled $ do writeIORef defEdgeDir False     
        rd2 `on` toggled $ do writeIORef defEdgeDir True
        -------------------------------------------------------         
 
        -- Return Created Vertical Box with Controls
        return optPane




-------------------------------------------------------------------------------------------------
---------------------------- PANEL DEFINING OPERATIONS ON GRAPHS --------------------------------
-------------------------------------------------------------------------------------------------


buildCmdPane = do 
        cmdPane <- vBoxNew False 0             
        l6 <- labelNew Nothing
        labelSetMarkup  l6 "<b>Graph Properties</b>"         
        labelSetJustify l6 JustifyLeft
        boxPackStart cmdPane l6 PackNatural 5
        b1 <- buttonNewWithLabel "Adjacency matrix"
        b2 <- buttonNewWithLabel "Incidence matrix"
        boxPackStart cmdPane b1 PackNatural 0
        boxPackStart cmdPane b2 PackNatural 0
        return cmdPane 


-------------------------------------------------------------------------------------------------
-------------------------------------- GRAPH DRAWING AREA ---------------------------------------
-------------------------------------------------------------------------------------------------


-- t = titulo, s = frase do label
createEntryDialog t s s' = do
  d <- dialogNew
  windowSetModal d True
  set d [windowTitle:=t]
  -- Prepare botton area
  y <- dialogAddButton d "Confirma" ResponseOk
  dialogAddButton      d "Cancela"  ResponseCancel
  -- Prepare upper area
  u <- dialogGetUpper d
  hb <- hBoxNew False 5
  l  <- labelNew $ Just $ s
  e  <- entryNew
  entrySetText e s'
  e `on` entryActivate $ buttonClicked y
  boxPackStartDefaults hb l
  boxPackStartDefaults hb e
  boxPackStartDefaults u hb
  widgetShowAll d
  return (d,e)




buildGraphDrawingArea :: IORef DiaGraph -> IORef NodeLayout -> IORef EdgeLayout -> IORef Bool -> IO DrawingArea
buildGraphDrawingArea dgraph defNodeLayout defEdgeLayout defEdgeDir = do
  
        -- Create drawing area
        da  <- drawingAreaNew
        widgetAddEvents   da   [AllEventsMask]
        widgetModifyBg    da   StateNormal (Color 65535 65535 65535)
        widgetSetCanFocus da   True
        
        -- Graph Editor State:

        -- undo/redo of graph editing
        undoDgraph   <- newIORef []
        redoDgraph   <- newIORef []
        movingDgraph <- newIORef False

        -- visualization of coordinate system
        zoom        <- newIORef (1)
        translation <- newIORef (0,0)
        transCoord  <- newIORef Nothing

        -- edge creation with right click: id of clicked node, coordinates of current cursor
        rightClick <- newIORef (Nothing :: Maybe (NodeId, Double,Double))
        createDirected <- newIORef False

        -- selection and selection movement
        cursor          <- newIORef (Nothing :: Maybe (Double,Double))               -- current position of mouse
        selection       <- newIORef (S.empty :: Selection)                           -- currently selected nodes and edges
        squareSelection <- newIORef (Nothing :: Maybe (Double,Double,Double,Double)) -- square selection of nodes
        clipBoard       <- newIORef (Nothing :: Maybe DiaGraph)                      -- saved graph to copy/paste


        -- Routine for updating undo/redo before a modification in current graph
        let storeUndo = do
              old <- readIORef  dgraph
              modifyIORef undoDgraph (old:)
              writeIORef  redoDgraph []

        -- Routine for loading graphs into the editor
        let loadDiaGraph (g,l) = do
              storeUndo
              writeIORef dgraph      (g,l)
              writeIORef zoom        1
              writeIORef translation (0,0)
              writeIORef transCoord  Nothing
              writeIORef rightClick  Nothing
              writeIORef cursor      Nothing
              writeIORef selection   S.empty
              writeIORef squareSelection Nothing
              writeIORef clipBoard   Nothing

        -- Handler : EXPOSURE EVENT
        da `on` exposeEvent $ do
            d <- eventWindow
            liftIO $ do
              dg      <- readIORef dgraph
              sel     <- readIORef selection
              rClick  <- readIORef rightClick
              createD <- readIORef createDirected
              sqSel   <- readIORef squareSelection
              z       <- readIORef zoom
              trans   <- readIORef translation
              drawWindowClear d
              renderWithDrawable d $ drawDiaGraph da dg sel rClick sqSel z trans createD
            return True



        -- Handler : BUTTON IS PRESSED
        da `on` buttonPressEvent $ do            
            (x,y) <- eventCoordinates
            b     <- eventButton
            m     <- eventModifier
            click <- eventClick
            liftIO $ do
              widgetGrabFocus da
              z       <- readIORef zoom
              (x0,y0) <- readIORef translation
              (g,l)   <- readIORef dgraph
              sel     <- readIORef selection
              let (x',y') = (x/z-x0,y/z-y0)
              case b of

                -- RIGHT BUTTON => Register node for future creation of edges
                RightButton -> do
                   case findNodeAt (x',y') (g,l) of
                     Nothing -> do
                                writeIORef rightClick Nothing
                                writeIORef createDirected False
                     Just v  -> do
                                writeIORef rightClick $ Just (v,x',y')
                                writeIORef createDirected (Control `elem` m)
                                putStrLn $ "Selected node for edge creation: " ++ (show v)
                   widgetQueueDraw da


                -- LEFT BUTTON => Select and unselect nodes and edges
                LeftButton -> do
                  case (findNodeAt (x',y') (g,l),
                        findEdgeAt (x',y') (g,l),
                        click == DoubleClick, -- test for label updating
                        Control `elem` m) of

                    -- Node found, double click => UPDATE NODE LABEL
                    (Just v,_,True,_)  -> do
                      let lab = fromMaybe "" (getNodeLabel v $ fst l)
                      (dia,e) <- createEntryDialog ("Nodo: " ++ (show v)) "Rótulo = " lab
                      j <- dialogRun dia
                      case j of
                        ResponseOk -> do
                          s <- entryGetText e
                          -- simulate text size to update layout
                          dims <- do
                               ctx <- widgetGetPangoContext da
                               pL  <- layoutText ctx $ s
                               (_,PangoRectangle px py pw ph) <- layoutGetExtents pL
                               return $ (pw,ph)
                          let l' = (setNodeLabel v s dims $ fst l, snd l)
                          storeUndo
                          writeIORef dgraph (g,l')
                          widgetQueueDraw da
                        _ -> return ()
                      widgetDestroy dia


                    -- Node found, Control not pressed => SELECT A UNIQUE NODE
                    (Just v,_,_,False)  -> do
                      if (N v) `S.member` sel
                         then return ()
                         else do
                              writeIORef selection $ S.singleton (N v)
                              putStrLn $ "Selected node " ++ (show v)

                    -- Node found, Control pressed  => TOGGLE NODE WITHIN SELECTION
                    (Just v,_,_,True)  -> do
                      if (N v) `S.member` sel
                         then do modifyIORef selection $ S.delete (N v)
                                 putStrLn $ "Unselected node " ++ (show v)
                         else do modifyIORef selection $ S.insert (N v)
                                 putStrLn $ "Added node " ++ (show v) ++ " to selection"

                   -- Edge found, double click => UPDATE Edge LABEL
                    (_,Just e,True,_)  -> do
                      let lab = fromMaybe "" (getEdgeLabel e $ snd l)
                      (dia,e') <- createEntryDialog ("Aresta: " ++ (show e)) "Rótulo = " lab
                      j <- dialogRun dia
                      case j of
                        ResponseOk -> do
                          s <- entryGetText e'
                          let l' = (fst l, setEdgeLabel e s $ snd l)
                          storeUndo
                          writeIORef dgraph (g,l')
                          widgetQueueDraw da
                        _ -> return ()
                      widgetDestroy dia




                    -- Edge found, Control not pressed => SELECT A UNIQUE EDGE
                    (_,Just e,_,False)  -> do
                      writeIORef selection $ S.singleton (E e)
                      putStrLn $ "Selected edge " ++ (show e)

                    -- Edge found, Control pressed  => UNSELECT EDGE (CURRENTLY WE DO NOT SELECT MORE THAN ONE EDGE AT A GIVEN TIME)
                    (_,Just e,_,True)  -> do
                      if (E e) `S.member` sel
                         then return ()
                         else do writeIORef selection S.empty
                                 putStrLn $ "Unselected edge " ++ (show e)

                    -- Clicked on white space, Control not pressed => START SQUARE SELECTION OF NODES
                    (_,_,_,b) -> do
                      C.when (not b) $ -- avoid accidental unselecting when ctrl is pressed
                        writeIORef selection S.empty
                      writeIORef squareSelection $ Just (x',y',x',y')
                      putStrLn $ "Started square selection"

                  writeIORef cursor $ Just (x',y')
                  widgetQueueDraw da


                -- MIDDLE BUTTON => IF CTRL ,THEN ALIGN NODES TO GRID, ELSE DO A TRANSLATION OF THE COORDINATE SYSTEM
                MiddleButton -> do
                  if not $ Control `elem` m
                    then do
                         writeIORef dgraph (g, straightenNodes l)
                         putStrLn "Align nodes to grid"
                         widgetQueueDraw da
                    else do
                         writeIORef transCoord $ Just (x',y')
                         putStrLn "Started translation coords"


                -- OTHER BUTTONS => NO-OPERATION
                _ -> return ()

              return True




        -- Handler => MOUSE IS MOVED
        da `on` motionNotifyEvent $ do
            (x,y) <- eventCoordinates
            m     <- eventModifierAll
            case (Button1 `elem` m, Button2 `elem` m,Button3 `elem` m) of

             -- Moving with left mouse pressed
             (True,_,_) -> do
               liftIO $ do
                 (g,l)   <- readIORef dgraph
                 z       <- readIORef zoom
                 (x0,y0) <- readIORef translation
                 sel     <- readIORef selection
                 mc      <- readIORef cursor
                 sq      <- readIORef squareSelection
                 let (x',y') = (x/z-x0,y/z-y0)
                 case (sq,mc) of

                   -- update square selection cursor, do not move selection
                   (Just _,_) -> do
                     modifyIORef squareSelection $ C.liftM $ (\(a,b,c,d)->(a,b,x',y'))
                     widgetQueueDraw da

                   -- Move selected graph items
                   (_,Just (ox,oy)) -> do
                     let (dx,dy) = (x'-ox,y'-oy)
                     if not $ S.null sel
                       then do
                            mv <- readIORef movingDgraph
                            C.when (not mv) $ do -- REGISTER CURRENT POSITION IN UNDO LIST
                                              storeUndo
                                              writeIORef movingDgraph True
                            writeIORef dgraph $ moveSelection (dx,dy) sel (g,l)
                            writeIORef cursor $ Just (x',y')
                       else return ()
                     widgetQueueDraw da

                   -- Neither of the previous cases
                   _ -> return ()

             -- Moving with middle mouse pressed: coordinate system translation
             (_,True,_) ->
               liftIO $ do
                        z       <- readIORef zoom
                        (x0,y0) <- readIORef translation
                        tCoord  <- readIORef transCoord
                        let (x',y') = (x/z-x0,y/z-y0)
                        case tCoord of
                          Nothing -> return ()
                          Just (ox,oy) -> do
                            let (dx,dy) = (x' - ox, y' - oy)
                            modifyIORef translation $ addPoint (dx,dy)
                            widgetQueueDraw da
                        return ()

             -- Moving with right mouse pressed: update edge creation tracker
             (_,_,True) -> do
               liftIO $ do
                 z       <- readIORef zoom
                 (x0,y0) <- readIORef translation
                 defED   <- readIORef defEdgeDir
                 let (x',y') = (x/z-x0,y/z-y0)
                 modifyIORef rightClick $ C.liftM $ (\(a,b,c) -> (a,x',y'))
                 writeIORef createDirected (xor (Control `elem` m) defED)
                 widgetQueueDraw da


             -- Moving without any button pressed: NOP
             _ -> return ()


            return True




        -- Handler: MOUSE WHEEL SCROLLED
        da `on` scrollEvent $ do
            dir <- eventScrollDirection
            m   <- eventModifier
            case dir of

              ScrollDown -> -- zoom out
                liftIO $ C.when (Control `elem` m) $ do
                          modifyIORef zoom (*0.95)
                          widgetQueueDraw da
              ScrollUp ->  -- zoom in
                liftIO $ C.when (Control `elem` m) $ do
                            modifyIORef zoom (*1.05)
                            widgetQueueDraw da
            return True



        -- Handler: BUTTON IS RELEASED
        da `on` buttonReleaseEvent $ do
            (x,y) <- eventCoordinates
            b     <- eventButton
            m     <- eventModifier
            liftIO $ do
              z       <- readIORef zoom
              (x0,y0) <- readIORef translation
              rClick  <- readIORef rightClick
              (g,l)   <- readIORef dgraph
              sel     <- readIORef selection
              let (n,e)   = (maxNode g, maxEdge g)
                  (x',y') = (x/z-x0,y/z-y0)
              case b of

                -- LEFT-BUTTON => if there are nodes within a square selection, select them.
                LeftButton  ->
                  do
                    sq <- readIORef squareSelection
                    case sq of

                      Nothing -> -- register that the movement of selection  has stopped (for undo/redo)
                                 do
                                 mv <- readIORef movingDgraph
                                 C.when (mv) $ writeIORef movingDgraph False

                      Just (a,b,u,v) -> do -- there is a square selection happening: select all nodes within it
                        let sel' = findNodesInRegion ((a,b),(u,v)) (g,l)
                        if Control `elem` m
                         then do -- if CTRL is pressed, selection is incremental

                          writeIORef selection $ (S.fromList $ map N sel') `S.union` sel
                          writeIORef squareSelection Nothing
                          putStrLn "Finishing square selection (union of selection)"
                         else do -- if CTRL is not pressed, new selection substitute old selection
                          writeIORef selection $ S.fromList $ map N sel'
                          writeIORef squareSelection Nothing
                          putStrLn "Finishing square selection"
                    widgetQueueDraw da


                -- MIDDLE BUTTON => clear translation movement
                MiddleButton -> do
                  writeIORef transCoord Nothing
                  widgetQueueDraw da


                -- RIGHT BUTTON => CREATE GRAPH ELEMENTS
                RightButton -> do
                  let cn = findNodeAt (x',y') (g,l)
                  case (rClick,cn) of

                    -- we do not do anything (button released between node and empty space)
                    (Nothing,Just nid) -> return ()
                    (Just _,Nothing) -> return ()


                    -- Create new node
                    (Nothing,Nothing)  -> do
                      defNL <- readIORef defNodeLayout
                      let defNL' = defNL {nodePos=(x',y')}                          
                          n'  = n+1
                          g'  = addNode  n'  g
                          str = calcLabel (nodeLabel defNL') n'
                      defNL'' <- updNodeLabel defNL' str da    
                      let l' = (M.insert n' defNL'' (fst l), snd l)
                      storeUndo     
                      writeIORef dgraph (g',l')    
                      putStrLn $ "Created node" ++ (show $ n')


                    -- Create new edge
                    (Just (nid,_,_),Just nid') -> do
                      defEL <- readIORef defEdgeLayout
                      defED <- readIORef defEdgeDir
                      if nid==nid'
                        then do -- create loop
                             let e' = e+1
                                 g' = addEdge e' nid nid (xor (Control `elem` m) defED) g
                                 mid = newLoopPos nid (g,l)
                                 str = calcLabel (edgeLabel defEL) e'
                                 defEL' = defEL {edgeLabel = str, edgePos=mid}
                                 l' = (fst l, M.insert e' defEL' (snd l))
                             storeUndo
                             writeIORef dgraph (g',l')
                             putStrLn $ "Created loop" ++ (show $ e')

                        else do -- create edge over two distinct nodes
                             let e' = e+1
                                 g' = addEdge e' nid nid' (xor (Control `elem` m) defED) g
                                 mid = newEdgePos nid nid' (g,l)
                                 str = calcLabel (edgeLabel defEL) e'
                                 defEL' = defEL {edgeLabel = str, edgePos=mid}
                                 l' = (fst l, M.insert e' defEL' (snd l))
                             storeUndo
                             writeIORef dgraph (g',l')
                             putStrLn $ "Created edge" ++ (show $ e')

                  -- Clear record of right-clicked nodes
                  writeIORef rightClick Nothing
                  writeIORef createDirected False
                  widgetQueueDraw da



                -- OTHER BUTTONS: NO-OP
                _  -> return ()

            return True






        -- Handler : KEYBOARD KEY IS PRESSED
        da `on` keyPressEvent $ do
            b <- eventKeyName
            m <- eventModifierAll
            liftIO $ do
                 (g,l)  <- readIORef dgraph
                 sel    <- readIORef selection
                 case b of

                  -- Delete selection
                  "Delete" -> do
                      let
                      C.when (not $ S.null sel) storeUndo
                      writeIORef dgraph $ delSelection sel (g,l)
                      writeIORef selection S.empty
                      widgetQueueDraw da
                      C.when (not $ S.null sel) $
                        putStrLn $ "Deleted selection: " ++ (show sel)

                  -- Reset zoom and translation
                  "0" -> C.when (Control `elem` m) $ do
                          writeIORef zoom 1
                          writeIORef translation (0,0)
                          widgetQueueDraw da

                  -- Select all nodes in the graph
                  "a" -> C.when (Control `elem` m) $ do
                          writeIORef selection $ S.fromList $ map N (nodes g)
                          widgetQueueDraw da


                  -- Copy subgraph induced by selected nodes in clipBoard
                  "c" -> C.when (Control `elem` m) $ do
                           if not $ S.null sel
                             then do
                                  let dg' = inducedSubDiagraph (selNodes sel)  (g,l)
                                  writeIORef clipBoard $ Just dg'
                                  putStrLn "Copy"
                             else writeIORef clipBoard Nothing

                  -- Cut subgraph induced by selected nodes in clipBoard
                  "x" -> C.when (Control `elem` m) $ do
                           if not $ S.null sel
                             then do
                                  writeIORef clipBoard $ Just $ inducedSubDiagraph (selNodes sel) (g,l)
                                  storeUndo
                                  writeIORef dgraph    $ delSelection sel (g,l)
                                  putStrLn "Cut"
                                  widgetQueueDraw da
                             else writeIORef clipBoard Nothing

                  -- Paste graph store in clipBoard
                  "v" -> C.when (Control `elem` m) $ do
                           clip <- readIORef clipBoard
                           case clip of
                             Nothing -> return ()
                             Just (g',(nl,el)) -> do
                                        dw <- widgetGetDrawWindow da
                                        (_,x,y,_) <- drawWindowGetPointerPos dw
                                        let (minX,minY,_,_) = layoutBBox (nl,el)
                                        let upd (a,b) = (20+a - minX,20+b - minY)
                                            l' = (M.map (liftNL upd) nl, el)
                                            (g'',l'') = diagrDisjointUnion (g,l) (g',l')
                                        storeUndo
                                        writeIORef dgraph  $ (g'',l'')
                                        writeIORef selection $ S.fromList $ map N $ (nodes g'') \\  (nodes g)

                                        putStrLn $ show (minX,minY)
                                        widgetQueueDraw da


                  --- Undo
                  "z" -> C.when (Control `elem` m) $ do
                           dg <- readIORef dgraph
                           u  <- readIORef undoDgraph
                           if null u
                              then return ()
                              else do
                                   writeIORef dgraph (head u)
                                   modifyIORef undoDgraph (tail)
                                   modifyIORef redoDgraph (dg:)
                                   writeIORef selection S.empty
                                   widgetQueueDraw da

                  --- Redo
                  "r" -> C.when (Control `elem` m) $ do
                           dg <- readIORef dgraph
                           r  <- readIORef redoDgraph
                           if null r
                              then return ()
                              else do
                                   modifyIORef undoDgraph (dg:)
                                   modifyIORef redoDgraph (tail)
                                   writeIORef dgraph (head r)
                                   writeIORef selection S.empty
                                   widgetQueueDraw da


                  --- Merge two nodes or contract one edge
                  "m" -> C.when (Control `elem` m) $ do
                           case S.toList sel of
                             -- Merge two nodes, preserving all edges between them
                             [N n1,N n2] -> do
                                        let (g',l') = mergeNodesDiagraph n1 n2 (g,l)
                                        storeUndo
                                        writeIORef dgraph  $ (g',l')
                                        writeIORef selection $ S.singleton $ N (min n1 n2)
                                        widgetQueueDraw da
                             -- Contract one edge: merge incident nodes and delete the edge
                             [E e] ->  case endPoints e g of
                                          Nothing -> return ()
                                          Just (n1,n2) -> do
                                                let (g',l') = mergeNodesDiagraph n1 n2 (g,l)
                                                    g'' = delEdge e g'
                                                    l'' = (fst l', M.delete e $ snd l')
                                                storeUndo
                                                writeIORef dgraph  $ (g'',l'')
                                                writeIORef selection $ S.empty
                                                widgetQueueDraw da
                             _ -> return ()

                  --- Create a node in the middle of an edge
                  "n" -> C.when (Control `elem` m) $ do
                           case S.toList sel of
                             -- Divide an edge, creating a new node between its edpoints
                             [E e] ->  case endPoints e g of
                                          Nothing -> return ()
                                          Just (n1,n2) -> do
                                                let (nc,ec) = (maxNode g,maxEdge g)
                                                let (p1,p2) = fromMaybe ((0,0),(0,0)) $
                                                                do r <- getNodePos n1 $ fst l
                                                                   s <- getNodePos n2 $ fst l
                                                                   return (r,s)
                                                    ang1 = angle p1 p2
                                                    dist1 = distance p1 p2
                                                    pt = fromMaybe (0,0) $
                                                           do (a,d) <- getEdgePos e $ snd l
                                                              return $ pointAt (ang1+a) (d*dist1) p1
                                                    n3 = nc + 1
                                                    e1 = ec + 1
                                                    e2 = ec + 2
                                                    g' = addEdge e1 n1 n3 False $
                                                           addEdge e2 n3 n2 False $
                                                             addNode n3 $
                                                               delEdge e g
                                                    l' = (M.insert n3 (wrapDefNodeLayout pt) $ fst l,
                                                          M.insert e1 (wrapDefEdgeLayout (0,0.5)) $
                                                            M.insert e2 (wrapDefEdgeLayout (0,0.5)) $
                                                             snd l)
                                                storeUndo
                                                writeIORef dgraph  $ (g',l')
                                                widgetQueueDraw da

                             _ -> return ()



                  -- Examples

                  "q" -> do defNL <- readIORef defNodeLayout
                            defEL <- readIORef defEdgeLayout
                            dg <- createDefLayout (grCycle 30) defNL defEL da
                            loadDiaGraph $ circLayout dg
                            widgetQueueDraw da

                  "w" -> do defNL <- readIORef defNodeLayout
                            defEL <- readIORef defEdgeLayout
                            dg <- createDefLayout (grGrid 10 5) defNL defEL da
                            loadDiaGraph $ circLayout dg
                            widgetQueueDraw da


                  "e" -> do loadDiaGraph $ doubleCircLayout $ grGenPetersen 5 2
                            widgetQueueDraw da

                  "t" -> do loadDiaGraph $ doubleCircLayout $ grGenPetersen 50 17
                            widgetQueueDraw da


                  -- Layouters

                  "1" -> do loadDiaGraph $ circLayout (g,l)
                            widgetQueueDraw da

                  "2" -> do loadDiaGraph $ doubleCircLayout $ g
                            widgetQueueDraw da

                  "3" -> do loadDiaGraph $ gridLayout $ g
                            widgetQueueDraw da

                  "4" -> do loadDiaGraph $ pathLayout $ g
                            widgetQueueDraw da

                  "5" -> print $ adjacencyMatrix g


                  -- Input/Output

                  "o" -> do --openDiaGraph "new.gst" dgraph da  storeUndo
                            return ()

                  "s" -> saveDiaGraph (g,l)

                  "p" -> putStrLn $ diagrToTikz (g,l)

                  _ -> return ()
            return True




        return da



---------------------- END OF GRAPH DRAWING AREA DEFINITION ------------------------------------



--- Main Function
main = do
        initGUI

        -- Main Window
        win <- windowNew
        set win [ windowTitle:="Graph Editor",
                  windowDefaultWidth:=640,
                  windowDefaultHeight:=480 ]
        onDestroy win mainQuit


        -- New node state
        defNodeLayout <- newIORef $ 
          NodeLayout { nodePos = (0,0), nodeShape = GLCircle 5, nodeColor = (0.84,0.84,0.84), nodeLabel = "" }
                             
        -- New edge default state                    
        defEdgeLayout <- newIORef $ 
          EdgeLayout { edgePos=(0,0), edgeColor=(0,0,1), edgeLabel="" }            
                             
        -- New edge default direction                    
        defEdgeDir <- newIORef $ False                    

        -- Current diagraph
        dgraph  <- newIORef (empty,(M.empty,M.empty))


         
        -- Vbox for main components of the interface
        vbox <- vBoxNew False 0
        widgetSetCanFocus vbox  True
        containerAdd win vbox
                  
        -- Menubar
        maybeMenubar <- buildMaybeMenubar 
        case maybeMenubar of
          (Just x) -> boxPackStart vbox x PackNatural 0
          Nothing  -> error "Cannot get menubar from string."
        
        -- Main Screen Hbox
        hbox <- hBoxNew False 5
        widgetSetCanFocus hbox  True
        boxPackStart vbox hbox PackGrow 0
        
        -- Opt Pane
        optPane <- buildOptPane dgraph defNodeLayout defEdgeLayout defEdgeDir
        boxPackStart hbox optPane PackNatural 0
        
        -- Graph Drawing Area
        da <- buildGraphDrawingArea dgraph defNodeLayout defEdgeLayout defEdgeDir
        widgetSetCanFocus da  True
        boxPackStart hbox da PackGrow 0
        widgetGrabFocus da
        --containerAdd win da

        -- Cmd Pane
        cmdPane <- buildCmdPane 
        boxPackStart hbox cmdPane PackNatural 0

        -- "Status Toolbar"
        stbar <- labelNew (Just "Left Mouse Button: select nodes/edges  |  Right Mouse Button: create nodes/edges")
        boxPackStart vbox stbar PackNatural 5

        -- main loop
        widgetShowAll win
        mainGUI


-------------------------------------------------------------------------------------------------
---------------------------------------- OPENDIAGRAPH  ------------------------------------------
-------------------------------------------------------------------------------------------------


openDiaGraph s dgraph da storeUndo = do
             xg <-runX ( readDocument [] s
                         >>> putXmlTree "-"
                         >>> getGraph )
             let g = processGraph xg
             print $ xg
             print $ g
             storeUndo
             --writeIORef dgraph $ circLayout $ g
             widgetQueueDraw da

saveDiaGraph (g,l) = return ()



{-  Future stuff

   Here we define the Layout datatype, which corresponds to a mapping
   between identifiers for nodes and edges and their graphical representation

-}





{- To Do List

>> reorganize graph layout:
     - (done) loops in polar coords
     - +- flexible forms (shape detection in clicking)
     - => label editing
     - advanced shapes
     - simple rendering
>> +- : XML open/save
>> +- : Tikz export
>> coordinate transformations for canvas
     * +- : translate viewport,
     * cartesian coords (-y,centered origin),
     * guiding rule)
>> (done) copy/paste/cut
>> (done) induced subgraph
>> fix layouts for pseudographs (loops, multiples edges)
>> define arrow tips
>> spring layouts
>> labels/weigths
>> high-performance editor
>> graph properties window (degrees, connectivity, etc.)
>> graph coloring interface
>> graph routing  interface
>> graph counting interface
>> graph library  interface (famous graphs in standard drawing)
>> graph algorithms (animation)
>>



Features

fast vs beautiful
edit vs visualize
grid snapping + layouters
digraphs
bipartite
coloring
typed graphs
labelled edges, nodes
attributed edges, nodes
save graph (algebraic, visual)
gui design / component

-}









--- Graph drawing routines

data VisualElem = Node (Double,Double) NodeLayout Bool   -- Pos, Layout, Selected?
                | Loop (Double,Double)  (Double,Double) EdgeLayout Bool Bool -- Pos, Midpoint, Layout, Directed?, Selected?,
                | Edge (Double,Double)  (Double,Double) (Double,Double) EdgeLayout Bool Bool -- Init, Midpoint, End, Layout, Directed?, Selected?
                | EdgeCreation (Double,Double) (Double,Double) (Maybe (Double,Double)) Bool -- Start, End, MidPoint of tgt node (if any), Directed?



setColorEdgeHandler b (r',g',b') =
  let ch c = if c<0.5 then c+0.4 else c-0.4 
      fl c = 0.8 * c
  in if b
       then setSourceRGB (fl $ ch r') (fl $ ch g') (fl $ ch b') >> setLineWidth 2 
       else setSourceRGB (fl r') (fl g') (fl b') >> setLineWidth 2


setColorEdge b (r',g',b') =
  let ch c = if c<0.5 then c+0.4 else c-0.4 
  in if b
    then setSourceRGB (ch r') (ch g') (ch b') >> setLineWidth 2
    else setSourceRGB r' g' b' >> setLineWidth 1


setColorNode b (r',g',b') =
  let ch c = if c<0.5 then c+0.4 else c-0.4 
  in if b
    then setSourceRGB (ch r') (ch g') (ch b') >> setLineWidth 1
    else setSourceRGB r' g' b'  >> setLineWidth 1


draw :: DrawingArea -> VisualElem -> Render ()

draw da (Loop (x,y) (a,d) lay dir s) = do
  let -- a = angle (x,y) (f,g)
      -- d = distance (f,g) (x,y)
      (f,g) = pointAt a d (x,y)
      p1 = pointAt (a+pi/8) (d/8) (x,y)
      p2 = pointAt (a+pi/2) (d/1.5) (f,g)
      p1' = pointAt (a-pi/8) (d/8) (x,y)
      p2' = pointAt (a-pi/2) (d/1.5) (f,g)
  moveTo x y
  curveTo (fst p1) (snd p1) (fst p2) (snd p2) f g
  moveTo x y
  curveTo (fst p1') (snd p1') (fst p2') (snd p2') f g
  setColorEdge s (edgeColor lay)
  stroke
  if not dir
    then do
         --setSourceRGB 0 0 0
         setColorEdgeHandler s (edgeColor lay)
         arc f g 1 0 (2*pi)
         fill
    else do
         let a = (3*pi/2) + (angle (f,g) (x,y))
             d = distance (f,g) (x,y)
             (x1,y1) = pointAt a          4 (f,g)
             (x2,y2) = pointAt (a+4*pi/6) 3 (f,g)
             (x3,y3) = pointAt (a-4*pi/6) 3 (f,g)
         moveTo x1 y1
         lineTo x2 y2
         lineTo x3 y3
         lineTo x1 y1
         --setSourceRGB 0 0 0 
         setColorEdgeHandler s (edgeColor lay)
         fill
  -- draw label
  let lab = edgeLabel lay
  if null lab
    then return ()
    else do

     -- prepare label
     ctx <- liftIO $ widgetGetPangoContext da
     pL  <- liftIO $ layoutText ctx $ lab
     (_,PangoRectangle px py pw ph) <- liftIO $ layoutGetExtents pL
     -- draw label

     let
         a' =  angle (x,y) (f,g) + (pi/2)
         (x0,y0) = multPoint (quadrant a') (pw/2,ph/2)
         minD = (abs $ tan(a')*x0 + y0) / sqrt(tan(a')*tan(a') + 1)
         labelPos = pointAt (-pi/2+a') (minD+2) (f,g)
     moveTo (fst labelPos - pw/2) (snd labelPos - ph/2)
     setSourceRGB 0 0 0
     showLayout pL

draw da (Edge (x,y) (f,g) (u,v) lay dir s) = do
  let (m,n) = midpoint (x,y) (u,v)
  if (f,g) == (m,n)
    then do
         moveTo x y
         --lineTo f g
         lineTo u v
         setColorEdge s (edgeColor lay)
         stroke
    else do
         let aglobal = angle (x,y) (u,v)
             d = distance (x,y) (u,v)
             p1 = pointAt (pi+aglobal) (d/4) (f,g)
             p2 = pointAt (aglobal)    (d/4) (f,g)
         moveTo x y
         curveTo x y (fst p1) (snd p1) f g
         curveTo (fst p2) (snd p2) u v u v
         setColorEdge s (edgeColor lay)
         stroke
  if not dir
    then do
         --setSourceRGB 0 0 0
         setColorEdgeHandler s (edgeColor lay)
         arc f g 1 0 (2*pi)
         fill
    else do
         let a = angle (x,y) (u,v)
             (x1,y1) = pointAt a          5 (f,g)
             (x2,y2) = pointAt (a+2*pi/3) 4 (f,g)
             (x3,y3) = pointAt (a-2*pi/3) 4 (f,g)
         moveTo x1 y1
         lineTo x2 y2
         lineTo x3 y3
         lineTo x1 y1
         -- setSourceRGB 0 0 0
         setColorEdgeHandler s (edgeColor lay)
         fill
     -- draw label
  let lab = edgeLabel lay
  if  null lab
    then return ()
    else do
     -- prepare label
     ctx <- liftIO $ widgetGetPangoContext da
     pL  <- liftIO $ layoutText ctx $ lab
     (_,PangoRectangle px py pw ph) <- liftIO $ layoutGetExtents pL
     -- draw label
     let a' =  angle (x,y) (u,v)
         (x0,y0) = multPoint (quadrant a') (pw/2,ph/2)
         minD = (abs $ tan(a')*x0 + y0) / sqrt(tan(a')*tan(a') + 1)
         labelPos = pointAt (-pi/2+a') (minD+2) (f,g)
     moveTo (fst labelPos - pw/2) (snd labelPos - ph/2)
     setSourceRGB 0 0 0
     showLayout pL



draw da (Node (x,y) lay s)  = do
  let rgb = (nodeColor lay)
      lab = nodeLabel lay
  -- draw node filling
  case nodeShape lay of
        GLCircle n -> arc x y n 0 (2*pi)
        GLSquare n -> rectangle (x-n/2) (y-n/2) n n
        GLRectangle m n -> rectangle (x-m/2) (y-n/2) m n
  setColorNode s rgb
  fill
  -- draw node line
  case nodeShape lay of
        GLCircle n -> arc x y n 0 (2*pi)
        GLSquare n -> rectangle (x-n/2) (y-n/2) n n
        GLRectangle m n -> rectangle (x-m/2) (y-n/2) m n
  setLineWidth 1
  setSourceRGB 0 0 0
  stroke
  -- draw label
  C.when (not $ null lab) $ do
     -- prepare label
     ctx <- liftIO $ widgetGetPangoContext da
     pL  <- liftIO $ layoutText ctx $ lab
     (_,PangoRectangle px py pw ph) <- liftIO $ layoutGetExtents pL
     moveTo (x-pw/2) (y-ph/2)
     setSourceRGB 0 0 0
     showLayout pL



draw da (EdgeCreation (x,y) (u,v) mp dir) = do
  setSourceRGB 0 0.8 0
  arc x y 6 0 (2*pi)
  fill
  moveTo x y
  lineTo u v
  stroke
  case mp of
    Nothing -> return ()
    Just (a,b) -> do
      arc a b 6 0 (2*pi)
      fill
  C.when dir $ do
    let (f,g) = midpoint (x,y) (u,v)
        a = angle (x,y) (u,v)
        (x1,y1) = pointAt a          5 (f,g)
        (x2,y2) = pointAt (a+2*pi/3) 4 (f,g)
        (x3,y3) = pointAt (a-2*pi/3) 4 (f,g)
    moveTo x1 y1
    lineTo x2 y2
    lineTo x3 y3
    lineTo x1 y1
    setSourceRGB 0 0.8 0
    fill





drawDiaGraph :: DrawingArea -> DiaGraph -> Selection -> Maybe (NodeId,Double,Double) -> Maybe (Double,Double,Double,Double) -> Double -> (Double,Double) -> Bool -> Render ()
drawDiaGraph da (g,l) sel rClick sqSel z trans ctrlPressed = do
  let edgePoints = mapMaybe
                        (\e-> do (s,t,dir) <- M.lookup e (edgeMap g)
                                 init <- getNodePos s $ fst l
                                 --t    <- tgt e g
                                 if s/=t
                                    then do end  <- getNodePos t $ fst l
                                            edgeLayout <- M.lookup e $ snd l
                                            let (ang,d) = edgePos edgeLayout
                                            let ang' = angle init end
                                            let mid = pointAt (ang'+ang) (d) $ midpoint init end
                                            return $ Edge init mid end edgeLayout dir (E e `S.member` sel)
                                    else do edgeLayout <- M.lookup e $ snd l
                                            let mid = edgePos edgeLayout
                                            return $ Loop init mid edgeLayout dir (E e `S.member` sel))
                        (edges g)
      nodePoints = mapMaybe
                        (\v-> do nodeLayout <- M.lookup v $ fst l
                                 p <- getNodePos v $ fst l
                                 return $ Node p nodeLayout (N v `S.member` sel)) $
                        (nodes g)
      edgeCreationPoints = maybeToList $
                             do (nid,u,v)  <- rClick
                                (x,y) <- getNodePos nid $ fst l
                                --(u,v) <- cur
                                return $ EdgeCreation (x,y) (u,v)
                                          (do n <- findNodeAt (u,v) (g,l)
                                              p <- getNodePos n $ fst l
                                              return $ p)
                                          ctrlPressed
  identityMatrix
  scale z z
  translate (fst trans) (snd trans)
  setLineWidth 1
  mapM_ (draw da) edgePoints         -- draw edges and loops
  mapM_ (draw da) nodePoints         -- draw nodes
  mapM_ (draw da) edgeCreationPoints -- draw edge creation selection
  -- draw square selection
  fromMaybe (return()) $ do
    (a,b,u,v) <- sqSel
    return $ do setSourceRGBA 1 0 0 0.15
                rectangle a b (u-a) (v-b)
                fill




--- Selection-related functions

data Select = N NodeId | E EdgeId
            deriving (Eq,Ord,Read,Show)

type Selection = S.Set Select




moveSelection :: (Double,Double) -> Selection -> DiaGraph -> DiaGraph
moveSelection d sel (g,l) =
  (g, S.fold (moveSelect d sel) l sel)
  where
  moveSelect :: (Double,Double) -> Selection -> Select -> GraphLayout -> GraphLayout
  moveSelect d sel (N n) (nl,el) = (M.adjust (liftNL (addPoint d)) n nl, el)

  moveSelect d sel (E e) (nl,el) = (nl, M.adjust (liftEL f) e el)
    where
    f = fromMaybe id
             (do  (nid,nid')  <- endPoints e g
                  if nid /= nid'
                     then do
                          p1 <- getNodePos nid  nl
                          p2 <- getNodePos nid' nl
                          (ae,de) <- getEdgePos e el
                          let pmid = midpoint p1 p2
                              (ang,dist) = toPolarFrom p1 p2
                              pEdge = pointAt (ae+ang) (de) $ pmid
                              pEdge' = (addPoint d pEdge)
                          return $ const $ ((angle pmid pEdge')-ang, ((distance pmid pEdge')))
                     else do 
                          p1      <- getNodePos nid  nl
                          (ae,de) <- getEdgePos e    el
                          let
                              pEdge' = addPoint d $ pointAt ae de p1
                              (a',d') = toPolarFrom p1 pEdge'
                          return $ const $  (a',d'))





selNodes sel = S.fold (\a m -> case a of {(N n)->(n:m);(E e)->m}) [] sel

selEdges sel = S.fold (\a m -> case a of {(N n)->m;(E e)->(e:m)}) [] sel



delSelection :: Selection -> DiaGraph -> DiaGraph
delSelection sel (g,l) =
  let
     nds = (selNodes sel)
     eds = (selEdges sel)
     dg' =  foldr diagrDelNode
              (foldr diagrDelEdge (g,l) eds)
              nds
  in dg'



--- Query canvas to find a node in a given point

findNodeAt :: (Double,Double) -> DiaGraph -> Maybe NodeId
findNodeAt (x,y) (g,(nl,el)) =
  M.foldrWithKey (\v-> \nl -> \m ->
                   let (a,b) = nodePos nl in
                   case nodeShape nl of
                     GLCircle n ->
                        if distance (x,y) (a,b) <= n
                          then Just v
                          else m
                     GLSquare n ->
                        if abs (x-a) <= (n/2) && abs (y-b) <= (n/2)
                          then Just v
                          else m
                     GLRectangle n k ->
                        if abs (x-a) <= (n/2) && abs (y-b) <= (k/2)
                          then Just v
                          else m)
                 Nothing  nl

findNodesInRegion :: ((Double,Double),(Double,Double)) -> DiaGraph -> [NodeId]
findNodesInRegion reg (g,(nl,el)) =
  M.foldrWithKey (\v-> \nl -> \m ->
                   let (a,b) = nodePos nl in
                   if isPointWithin reg (a,b)
                     then (v:m)
                     else m)
                 []  nl



findEdgeAt :: (Double,Double) -> DiaGraph -> Maybe EdgeId
findEdgeAt (x,y) (g,(nl,el)) =
  M.foldrWithKey (\e-> \edl -> \m ->
                   let (a,b) = edgePos edl in
                   case do (n1,n2) <- endPoints e g
                           p1 <- getNodePos n1 nl
                           p2 <- getNodePos n2 nl
                           (c,d) <- getEdgePos e el
                           let mid' = if n1 == n2
                                        then pointAt c d p1
                                        else pointAt (angle p1 p2 + c)
                                                     (d)
                                                     $ midpoint p1 p2
                           return mid'
                        of

                     Just (v,w) -> if distance (x,y) (v,w) <= 5
                                   then Just e
                                   else m
                     Nothing -> m)
                 Nothing  el
