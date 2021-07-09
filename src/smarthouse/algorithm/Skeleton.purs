module SmartHouse.Algorithm.Skeleton where

import Prelude

import Algorithm.MeshFlatten (_vertex)
import Control.Monad.RWS (get)
import Data.Array ((!!))
import Data.Array as Arr
import Data.Compactable (compact)
import Data.Filterable (filter)
import Data.Foldable (class Foldable, foldl, minimumBy, traverse_)
import Data.Lens (view, (^.))
import Data.List (List(..), concatMap, foldM, fromFoldable, singleton)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.PQueue (PQueue)
import Data.PQueue as PQ
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Triple (Triple(..))
import Data.Tuple (Tuple(..), snd)
import Editor.Common.Lenses (_distance, _position)
import Effect (Effect)
import Effect.Class (liftEffect)
import Math (abs)
import Math.Line (_direction, _origin, intersection)
import Math.LineSeg (LineSeg, _start, direction, distToLineSeg)
import Math.LineSeg as S
import Math.Utils (approxSame, epsilon)
import Model.Polygon (Polygon)
import Model.UUID (idLens)
import SmartHouse.Algorithm.Edge (Edge, _leftBisector, _line, _rightBisector)
import SmartHouse.Algorithm.Event (EdgeE, EdgesE, PointEvent(..), SplitE, _intersection, _oppositeEdge, _vertexA, _vertexB, _vertexC, distance, edgeE, edgesE, intersectionPoint, splitE)
import SmartHouse.Algorithm.LAV (LAV, SLAV, _edges, _lavs, _vertices, addLav, delLav, emptySLAV, eventValid, getLav, invalidateVertex, lavFromVertices, length, nextVertex, prevVertex, runSLAV, unifyThreeVerts, unifyVerts, updateLav, verticesFromTo)
import SmartHouse.Algorithm.Ray (ray)
import SmartHouse.Algorithm.VertInfo (_bisector, _cross, _isReflex)
import SmartHouse.Algorithm.VertNode (VertNode, mkVertNode, vertNodeFromVertex)
import SmartHouse.Algorithm.Vertex (Vertex, _lavId, _leftEdge, _rightEdge, vertexFrom)
import SmartHouse.HouseTracer (almostParallel)
import Smarthouse.Algorithm.Subtree (Subtree, SubtreeType(..), subtree)
import Three.Math.Vector (class Vector, Vector3, dist, normal, (<**>), (<+>), (<->), (<.>))
import Three.Math.Vector as V


-- a potential b is at the intersection of between our own bisector and the bisector of the
-- angle between the tested edge and any one of our own edges.
-- we choose the "less parallel" edge (in order to exclude a potentially parallel edge)
intersectP :: LineSeg Vector3 -> LineSeg Vector3 -> Edge -> Maybe Vector3
intersectP lEdge rEdge e = let eVec     = direction $ e ^. _line
                               leftdot  = abs $ direction lEdge <.> eVec
                               rightdot = abs $ direction rEdge <.> eVec
                               selfEdge = if leftdot < rightdot then lEdge else rEdge
                           in S.intersection selfEdge (e ^. _line)

-- locate candidate b
locateB :: Vertex -> Tuple Edge Vector3 -> Maybe (Tuple Edge Vector3)
locateB v (Tuple e i) = let linVec   = normal $ v ^. _position <-> i
                            edVec    = direction $ e ^. _line
                            edVec2   = if linVec <.> edVec < 0.0 then edVec <**> (-1.0) else edVec
                            -- bisector of the tested edge e and self edge of v
                            bisecVec = edVec2 <+> linVec
                        in if V.length bisecVec == 0.0
                           then Nothing
                           else Tuple e <$> intersection (ray i bisecVec) (v ^. _bisector)

-- check eligibility of b
-- valid b should lie within the area limited by the edge and the bisectors of its two vertices
validB :: Tuple Edge Vector3 -> Boolean
validB (Tuple e b) = let lb = e ^. _leftBisector
                         rb = e ^. _rightBisector
                         xleft = _cross (normal $ lb ^. _direction)
                                        (normal $ b <-> lb ^. _origin) > (- epsilon)
                         xright = _cross (normal $ rb ^. _direction)
                                         (normal $ b <-> rb ^. _origin) < epsilon
                         xedge = _cross (direction $ e ^. _line) (normal $ b <-> e ^. _line <<< _start) < epsilon
                     in xleft && xright && xedge

-- next event for a reflex vertex
nextEvtForReflex :: Vertex -> SLAV (List PointEvent)
nextEvtForReflex v = do
    originEdges <- view _edges <$> get
    let lEdge = v ^. _leftEdge <<< _line
        rEdge = v ^. _rightEdge <<< _line

        -- check if an edge is the left/right edge of the vertex v
        notNearby e = not $ e ^. _line == lEdge || e ^. _line == rEdge
        -- all edges not connected to vertex v
        edges = fromFoldable $ filter notNearby originEdges
        
        notV i = not $ approxSame i $ v ^. _position

        -- find a valid intersection point
        validIntersectP :: Edge -> Maybe (Tuple Edge Vector3)
        validIntersectP e = intersectP lEdge rEdge e >>= (\i -> if notV i then Just $ Tuple e i else Nothing)

        findValidB :: Tuple Edge Vector3 -> Maybe (Tuple Edge Vector3)
        findValidB t = locateB v t >>= (\r -> if validB r then Just r else Nothing)

        mkSplitEvt :: Tuple Edge Vector3 -> PointEvent
        mkSplitEvt (Tuple e b) = splitE (distToLineSeg b (e ^. _line)) b v e

    pure $ compact $ (validIntersectP >=> findValidB >>> map mkSplitEvt) <$> edges

-- next event for a vertex
nextEvent :: LAV -> Vertex -> SLAV (Maybe PointEvent)
nextEvent lav v = do
    evts <- if v ^. _isReflex then nextEvtForReflex v else pure Nil
    let prevV = prevVertex v lav
        nextV = nextVertex v lav
        -- intersection of a vertex's bisector with v's bisector
        intersectWith = view _bisector >>> intersection (v ^. _bisector)
        iPrev = prevV >>= intersectWith
        iNext = nextV >>= intersectWith

        mkPrevEdgeEvt e pv pi = edgeE (distToLineSeg pi e) pi pv v
        mkNextEdgeEvt e nv ni = edgeE (distToLineSeg ni e) ni v nv
        mkEdgesEvt le re pv nv i = let ld = distToLineSeg i le
                                       rd = distToLineSeg i re
                                   in edgesE (min ld rd) i pv v nv

        -- if prev and next intersection is very close, and left edge of prev vert
        -- is almost parallel to the right edge of next vert, BUT NOT the same,
        -- then it's a MergedNode, otherwise it should be NormalNode
        prevLeftE = view _leftEdge <$> prevV
        nextRightE = view _rightEdge <$> nextV

        isSame = prevLeftE == nextRightE
        isPara = fromMaybe false $ almostParallel <$> (view _line <$> prevLeftE) <*> (view _line <$> nextRightE)
    
        es = if closeEnough iPrev iNext && not isSame && isPara
             then [mkEdgesEvt (v ^. _leftEdge <<< _line) (v ^. _rightEdge <<< _line) <$> prevV <*> nextV <*> iPrev]
             else [mkPrevEdgeEvt (v ^. _leftEdge <<< _line) <$> prevV <*> iPrev,
                   mkNextEdgeEvt (v ^. _rightEdge <<< _line) <$> nextV <*> iNext]
                        
        allEvts = append (fromFoldable (compact es)) evts
        distF e = dist (v ^. _position) (intersectionPoint e)
    pure $ minimumBy (comparing distF) allEvts


closeEnough :: Maybe Vector3 -> Maybe Vector3 -> Boolean
closeEnough v1 v2 = fromMaybe false $ f <$> v1 <*> v2
    where f va vb = dist va vb < 0.1

-- check if an edge event's two vertices A, B and A's predecessor C forms a triangle ABC
isTriangle :: EdgeE -> LAV -> Boolean
isTriangle e lav = prevVertex (e ^. _vertexA) lav == nextVertex (e ^. _vertexB) lav

-- check if an EdgesE event's vertices A, B, C and A's predecessor D forms a polygon ABCD
isRectangle :: EdgesE -> LAV -> Boolean
isRectangle e lav = prevVertex (e ^. _vertexA) lav == nextVertex (e ^. _vertexC) lav

lavForEvt :: PointEvent -> SLAV (Maybe LAV)
lavForEvt (EdgeEvent e) = getLav $ e ^. _vertexA <<< _lavId
lavForEvt (EdgesEvent e) = getLav $ e ^. _vertexA <<< _lavId
lavForEvt (SplitEvent e) = getLav $ e ^. _vertex <<< _lavId

-- handle PointEvents
handleEvent :: PointEvent -> SLAV (Maybe (Tuple Subtree (List PointEvent)))
handleEvent (EdgeEvent e)  = handleEdgeEvent e
handleEvent (EdgesEvent e) = handleEdgesEvent e
handleEvent (SplitEvent e) = handleSplitEvent e

-- handle edge event
handleEdgeEvent :: EdgeE -> SLAV (Maybe (Tuple Subtree (List PointEvent)))
handleEdgeEvent e = getLav (e ^. _vertexA <<< _lavId) >>= traverse (handleEdgeEvent' e)

handleEdgeEvent' :: EdgeE -> LAV -> SLAV (Tuple Subtree (List PointEvent))
handleEdgeEvent' e lav =
    if isTriangle e lav
        then do let vs = lav ^. _vertices
                    sinks = fromFoldable $ vertNodeFromVertex <$> vs
                    edges = Set.fromFoldable $ Arr.concatMap (\v -> [v ^. _leftEdge, v ^. _rightEdge]) vs
                    
                -- this new Vertex is only used in the final subtree. only position and height is used.
                newV <- liftEffect $ mkVertNode (e ^. _intersection) (e ^. _distance)

                -- delete this LAV and invalidate all vertices in it
                delLav (lav ^. idLens)
                traverse_ invalidateVertex vs

                t <- liftEffect $ subtree NormalNode newV sinks (Set.toUnfoldable edges)
                
                pure $ Tuple t Nil
        else do let va = e ^. _vertexA
                    vb = e ^. _vertexB
                Tuple newLav newV <- liftEffect $ unifyVerts va vb (e ^. _intersection) (e ^. _distance) lav
                invalidateVertex va
                invalidateVertex vb

                let sinks = fromFoldable [vertNodeFromVertex va, vertNodeFromVertex vb]
                    edges = Set.fromFoldable [va ^. _leftEdge, va ^. _rightEdge, vb ^. _leftEdge, vb ^. _rightEdge]
                newEvt <- nextEvent newLav newV
                let evts = fromFoldable $ compact [newEvt]

                updateLav newLav (Just newV)

                t <- liftEffect $ subtree NormalNode (vertNodeFromVertex newV) sinks (Set.toUnfoldable edges)
                pure $ Tuple t evts


handleEdgesEvent :: EdgesE -> SLAV (Maybe (Tuple Subtree (List PointEvent)))
handleEdgesEvent e = getLav (e ^. _vertexA <<< _lavId) >>= traverse (handleEdgesEvent' e)

handleEdgesEvent' :: EdgesE -> LAV -> SLAV (Tuple Subtree (List PointEvent))
handleEdgesEvent' e lav =
    if isRectangle e lav
        then do let vs = lav ^. _vertices
                    sinks = fromFoldable $ vertNodeFromVertex <$> vs
                    edges = Set.fromFoldable $ Arr.concatMap (\v -> [v ^. _leftEdge, v ^. _rightEdge]) vs

                -- this new Vertex is only used in the final subtree. only position and height is used.
                newV <- liftEffect $ mkVertNode (e ^. _intersection) (e ^. _distance)

                -- delete this LAV and invalidate all vertices in it
                delLav (lav ^. idLens)
                traverse_ invalidateVertex vs

                t <- liftEffect $ subtree NormalNode newV sinks (Set.toUnfoldable edges)
                pure $ Tuple t Nil
        else do let va = e ^. _vertexA
                    vb = e ^. _vertexB
                    vc = e ^. _vertexC
                Triple newLav newV canHaveNewEvts <- liftEffect $ unifyThreeVerts va vb vc (e ^. _intersection) (e ^. _distance) lav
                invalidateVertex va
                invalidateVertex vb
                invalidateVertex vc

                let sinks = fromFoldable $ vertNodeFromVertex <$> [va, vb, vc]
                    edges = Set.fromFoldable [va ^. _leftEdge, va ^. _rightEdge,
                                              vb ^. _leftEdge, vb ^. _rightEdge,
                                              vc ^. _leftEdge, vc ^. _rightEdge]
                    nodeT = MergedNode (va ^. _leftEdge) (vc ^. _rightEdge)
                evts <- if canHaveNewEvts
                        then do
                            newEvt <- nextEvent newLav newV
                            pure $ fromFoldable $ compact [newEvt]
                        else pure Nil

                updateLav newLav (Just newV)

                t <- liftEffect $ subtree nodeT (vertNodeFromVertex newV) sinks (Set.toUnfoldable edges)
                pure $ Tuple t evts


handleSplitEvent :: SplitE -> SLAV (Maybe (Tuple Subtree (List PointEvent)))
handleSplitEvent e = findXY e >>= traverse (handleSplitEvent' e)

handleSplitEvent' :: SplitE -> Tuple Vertex Vertex -> SLAV (Tuple Subtree (List PointEvent))
handleSplitEvent' e (Tuple x y) = do
    let lavId  = e ^. _vertex <<< _lavId
        intPos = e ^. _intersection
        h      = e ^. _distance
        v      = e ^. _vertex
    v1 <- liftEffect $ vertexFrom lavId intPos h (v ^. _leftEdge) (e ^. _oppositeEdge) Nothing Nothing
    v2 <- liftEffect $ vertexFrom lavId intPos h (e ^. _oppositeEdge) (v ^. _rightEdge) Nothing Nothing

    lav <- getLav lavId
    let edges = Set.fromFoldable [v ^. _leftEdge, v ^. _rightEdge]
    
        eNext = lav >>= nextVertex v
        ePrev = lav >>= prevVertex v

    delLav lavId  -- delete original LAV
    
    Triple evts sinks es <- if lavId /= x ^. _lavId
                            then do  -- the split event actually merges two LAVs
                                lav1 <- getLav $ x ^. _lavId
                                let vs1 = Cons v1 $ fromMaybe Nil $ verticesFromTo x y <$> lav1
                                    vs2 = Cons v2 $ fromMaybe Nil $ verticesFromTo <$> eNext <*> ePrev <*> lav

                                    -- all vertices from the two LAVs and new v1, v2
                                    nvs = vs1 <> vs2
                                delLav $ x ^. _lavId
                                nLav <- liftEffect $ lavFromVertices nvs
                                processNewLAV nLav $ fromFoldable [v1, v2]
                            else do
                                let vs1 = Cons v1 $ fromMaybe Nil $ verticesFromTo <$> Just x <*> ePrev <*> lav
                                    vs2 = Cons v2 $ fromMaybe Nil $ verticesFromTo <$> eNext <*> Just y <*> lav
                                lav1 <- liftEffect $ lavFromVertices vs1
                                lav2 <- liftEffect $ lavFromVertices vs2

                                Triple evts1 sinks1 edges1 <- processNewLAV lav1 $ singleton v1
                                Triple evts2 sinks2 edges2 <- processNewLAV lav2 $ singleton v2
                                pure $ Triple (evts1 <> evts2) (sinks1 <> sinks2) (edges1 <> edges2)

    invalidateVertex v

    t <- liftEffect $ subtree NormalNode (vertNodeFromVertex v1) (Cons (vertNodeFromVertex v) sinks) (Set.toUnfoldable $ edges <> es)
    
    pure $ Tuple t evts


processNewLAV :: LAV -> List Vertex -> SLAV (Triple (List PointEvent) (List VertNode) (Set.Set Edge))
processNewLAV lav nvs = if length lav > 2
                        then do addLav lav nvs
                                evts <- traverse (nextEvent lav) nvs
                                pure $ Triple (compact evts) Nil Set.empty
                        else -- only 2 vertices in this LAV, collapse it
                            do let vs = lav ^. _vertices
                                   v  = vs !! 1
                                   sink = v
                                   edges = Set.fromFoldable $ compact $ [view _leftEdge <$> v, view _rightEdge <$> v]
                               traverse_ invalidateVertex vs
                               pure $ Triple Nil (vertNodeFromVertex <$> compact (singleton sink)) edges


findXY :: SplitE -> SLAV (Maybe (Tuple Vertex Vertex))
findXY e = get >>= getVS >>> foldM f Nothing
    where opEdge  = e ^. _oppositeEdge ^. _line
          opNorm  = direction opEdge
          opStart = opEdge ^. _start

          -- get all vertices in all LAVs
          getVS s = concatMap (fromFoldable <<< view _vertices) (M.values $ s ^. _lavs)

          f Nothing v    = testV opStart opNorm (e ^. _intersection) v
          f r@(Just _) v = pure r

-- test if a vertex is the right candidate for calculating X and Y
testV :: Vector3 -> Vector3 -> Vector3 -> Vertex -> SLAV (Maybe (Tuple Vertex Vertex))
testV eStart eNorm p v = do
    lav <- getLav $ v ^. _lavId

    let f t@(Tuple x y) = let xleft  = _cross (normal $ y ^. _bisector <<< _direction) (normal $ p <-> y ^. _position) >= (- epsilon)
                              xright = _cross (normal $ x ^. _bisector <<< _direction) (normal $ p <-> x ^. _position) <= epsilon
                          in if xleft && xright then Just t else Nothing

    pure $ if eNorm == direction (v ^. _leftEdge <<< _line) && eStart == v ^. _leftEdge <<< _line <<< _start
           then lav >>= prevVertex v >>= Tuple v >>> f
           else if eNorm == direction (v ^. _rightEdge <<< _line) && eStart == v ^. _rightEdge <<< _line <<< _start
                then lav >>= nextVertex v >>= flip Tuple v >>> f
                else Nothing


addEvtsToQueue :: forall f. Foldable f => PQueue Number PointEvent -> f PointEvent -> PQueue Number PointEvent
addEvtsToQueue = foldl (\q' e -> PQ.insert (distance e) e q')

-- Compute Straight Skeleton of a polygon
skeletonize :: forall v. Eq v => Vector v => Polygon v -> Effect (Tuple (List Subtree) (List Edge))
skeletonize = runSLAV do
    trees <- skeletonize'
    edges <- fromFoldable <<< view _edges <$> get
    pure $ Tuple trees edges

skeletonize' :: SLAV (List Subtree)
skeletonize' = do
    lavs <- view _lavs <$> get
    let calcEvts lav = traverse (nextEvent lav) (lav ^. _vertices)
    evts <- join <$> traverse calcEvts (Arr.fromFoldable lavs)
    let priEvts = (\e -> Tuple (distance e) e) <$> compact evts

        queue = PQ.fromFoldable priEvts

        getNext = pure <<< map snd <<< PQ.head
        validate e = eventValid e >>= (\v -> pure $ if v then Just e else Nothing)
            
        go Nothing out  = pure out
        go (Just q) out = do
            v <- emptySLAV
            if not (PQ.isEmpty q || v)
                then do res <- handleEvent <<<= validate <<<= getNext q
                        case res of
                            Nothing -> go (PQ.tail q) out
                            Just (Tuple arc es) -> go (Just $ addEvtsToQueue q es) (Cons arc out)
                else pure out

    go (Just queue) Nil


chainMaybe :: forall a b m. Monad m => (a -> m (Maybe b)) -> m (Maybe a) -> m (Maybe b)
chainMaybe f v = v >>= maybe (pure Nothing) f

infixr 5 chainMaybe as <<<=
