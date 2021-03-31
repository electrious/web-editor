module SmartHouse.Algorithm.Skeleton where

import Prelude

import Algorithm.MeshFlatten (_vertex)
import Control.Monad.RWS (get)
import Data.Array (foldl, (!!))
import Data.Array as Arr
import Data.Compactable (compact)
import Data.Filterable (filter)
import Data.Foldable (class Foldable, minimumBy, traverse_)
import Data.Lens (view, (^.))
import Data.List (List(..), concatMap, foldM, fromFoldable, singleton)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.PQueue (PQueue)
import Data.PQueue as PQ
import Data.Traversable (class Traversable, traverse)
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
import SmartHouse.Algorithm.Event (EdgeE, PointEvent(..), SplitE, _intersection, _oppositeEdge, _vertexA, _vertexB, distance, edgeE, intersectionPoint, splitE)
import SmartHouse.Algorithm.LAV (LAV, SLAV, _edges, _lavs, _vertices, addLav, delLav, emptySLAV, getLav, invalidateVertex, lavFromVertices, length, nextVertex, prevVertex, runSLAV, unifyVerts, updateLav, verticesFromTo)
import SmartHouse.Algorithm.Vertex (Vertex, _bisector, _cross, _isReflex, _lavId, _leftEdge, _rightEdge, ray, vertexFrom)
import Smarthouse.Algorithm.Subtree (Subtree, subtree)
import Three.Math.Vector (class Vector, Vector3, dist, normal, (<**>), (<+>), (<->), (<.>))
import Three.Math.Vector as V


-- a potential b is at the intersection of between our own bisector and the bisector of the
-- angle between the tested edge and any one of our own edges.
-- we choose the "less parallel" edge (in order to exclude a potentially parallel edge)
intersectP :: LineSeg Vector3 -> LineSeg Vector3 -> Edge -> Maybe Vector3
intersectP lEdge rEdge e = let eVec      = direction $ e ^. _line
                               leftdot   = abs $ direction lEdge <.> eVec
                               rightdot  = abs $ direction rEdge <.> eVec
                               selfEdge  = if leftdot < rightdot then lEdge else rEdge
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
                           else let bisector = ray i bisecVec
                                in Tuple e <$> intersection bisector (v ^. _bisector)

-- check eligibility of b
-- valid b should lie within the area limited by the edge and the bisectors of its two vertices
validB :: Tuple Edge Vector3 -> Boolean
validB (Tuple e b) = let xleft = _cross (normal $ e ^. _leftBisector <<< _direction)
                                        (normal $ b <-> e ^. _leftBisector <<< _origin) > (- epsilon)
                         xright = _cross (normal $ e ^. _rightBisector <<< _direction)
                                         (normal $ b <-> e ^. _rightBisector <<< _origin) < epsilon
                         xedge = _cross (direction $ e ^. _line) (b <-> e ^. _line <<< _start) < epsilon
                     in xleft && xright && xedge

-- next event for a reflex vertex
nextEvtForReflex :: Vertex -> SLAV (List PointEvent)
nextEvtForReflex v = do
    originEdges <- view _edges <$> get
    let lEdge = v ^. _leftEdge
        rEdge = v ^. _rightEdge

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
        pEvt = mkPrevEdgeEvt (v ^. _leftEdge) <$> prevV <*> iPrev
        mkNextEdgeEvt e nv ni = edgeE (distToLineSeg ni e) ni v nv
        nEvt = mkNextEdgeEvt (v ^. _rightEdge) <$> nextV <*> iNext

        allEvts = append (fromFoldable (compact [pEvt, nEvt])) evts
        distF e = dist (v ^. _position) (intersectionPoint e)
    pure $ minimumBy (comparing distF) allEvts



-- check if an edge event's two vertices A, B and A's predecessor C forms a triangle ABC
isTriangle :: EdgeE -> LAV -> Boolean
isTriangle e lav = prevVertex (e ^. _vertexA) lav == nextVertex (e ^. _vertexB) lav

lavForEvt :: PointEvent -> SLAV (Maybe LAV)
lavForEvt (EdgeEvent e) = getLav $ e ^. _vertexA <<< _lavId
lavForEvt (SplitEvent e) = getLav $ e ^. _vertex <<< _lavId

-- handle PointEvents
handleEvent :: PointEvent -> SLAV (Maybe (Tuple Subtree (List PointEvent)))
handleEvent (EdgeEvent e)  = handleEdgeEvent e
handleEvent (SplitEvent e) = handleSplitEvent e

-- handle edge event
handleEdgeEvent :: EdgeE -> SLAV (Maybe (Tuple Subtree (List PointEvent)))
handleEdgeEvent e = getLav (e ^. _vertexA <<< _lavId) >>= traverse (handleEdgeEvent' e)

handleEdgeEvent' :: EdgeE -> LAV -> SLAV (Tuple Subtree (List PointEvent))
handleEdgeEvent' e lav =
    if isTriangle e lav
        then do let vs = lav ^. _vertices
                    sinks = fromFoldable $ view _position <$> vs

                -- delete this LAV and invalidate all vertices in it
                delLav (lav ^. idLens)
                traverse_ invalidateVertex vs

                pure $ Tuple (subtree (e ^. _intersection) (e ^. _distance) sinks) Nil
        else do let va = e ^. _vertexA
                    vb = e ^. _vertexB
                Tuple newLav newV <- liftEffect $ unifyVerts va vb (e ^. _intersection) lav
                invalidateVertex va
                invalidateVertex vb

                let sinks = fromFoldable [va ^. _position, vb ^. _position]
                newEvt <- nextEvent newLav newV
                let evts = fromFoldable $ compact [newEvt]

                updateLav newLav newV
                
                pure $ Tuple (subtree (e ^. _intersection) (e ^. _distance) sinks) evts


handleSplitEvent :: SplitE -> SLAV (Maybe (Tuple Subtree (List PointEvent)))
handleSplitEvent e = findXY e >>= traverse (handleSplitEvent' e)

handleSplitEvent' :: SplitE -> Tuple Vertex Vertex -> SLAV (Tuple Subtree (List PointEvent))
handleSplitEvent' e (Tuple x y) = do
    let lavId  = e ^. _vertex <<< _lavId
        intPos = e ^. _intersection
    v1 <- liftEffect $ vertexFrom lavId intPos (e ^. _vertex <<< _leftEdge) (e ^. _oppositeEdge <<< _line) Nothing Nothing
    v2 <- liftEffect $ vertexFrom lavId intPos (e ^. _oppositeEdge <<< _line) (e ^. _vertex <<< _rightEdge) Nothing Nothing

    lav <- getLav lavId
    let v = e ^. _vertex
    
        eNext = lav >>= nextVertex v
        ePrev = lav >>= prevVertex v

    delLav lavId  -- delete original LAV
    
    Tuple evts sinks  <- if lavId /= x ^. _lavId
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

                             Tuple evts1 sinks1 <- processNewLAV lav1 $ singleton v1
                             Tuple evts2 sinks2 <- processNewLAV lav2 $ singleton v2
                             pure $ Tuple (evts1 <> evts2) (sinks1 <> sinks2)

    invalidateVertex v
    
    pure $ Tuple (subtree intPos (e ^. _distance) (Cons (v ^. _position) sinks)) evts


processNewLAV :: LAV -> List Vertex -> SLAV (Tuple (List PointEvent) (List Vector3))
processNewLAV lav nvs = if length lav > 2
                        then do addLav lav nvs
                                evts <- traverse (nextEvent lav) nvs
                                pure $ Tuple (compact evts) Nil
                        else -- only 2 vertices in this LAV, collapse it
                            do let vs = lav ^. _vertices
                                   sink = view _position <$> vs !! 1
                               traverse_ invalidateVertex vs
                               pure $ Tuple Nil (compact $ singleton sink)


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

    pure $ if eNorm == direction (v ^. _leftEdge) && eStart == v ^. _leftEdge <<< _start
           then lav >>= prevVertex v >>= Tuple v >>> f
           else if eNorm == direction (v ^. _rightEdge) && eStart == v ^. _rightEdge <<< _start
                then lav >>= nextVertex v >>= flip Tuple v >>> f
                else Nothing


addEvtsToQueue :: forall f. Foldable f => PQueue Number PointEvent -> f PointEvent -> PQueue Number PointEvent
addEvtsToQueue = foldl (\q' e -> PQ.insert (distance e) e q')

-- Compute Straight Skeleton of a polygon
skeletonize :: forall f v. Functor f => Foldable f => Traversable f => Eq v => Vector v => f (Polygon v) -> Effect (List Subtree)
skeletonize = runSLAV skeletonize'

skeletonize' :: SLAV (List Subtree)
skeletonize' = do
    lavs <- view _lavs <$> get
    let calcEvts lav = traverse (nextEvent lav) (lav ^. _vertices)
    evts <- join <$> traverse calcEvts (Arr.fromFoldable lavs)
    let priEvts = (\e -> Tuple (distance e) e) <$> compact evts

        queue = PQ.fromFoldable priEvts

        go Nothing out  = pure out
        go (Just q) out = do
            v <- emptySLAV
            if not (PQ.isEmpty q || v)
                then do res <- join <$> traverse handleEvent (snd <$> PQ.head q)
                        case res of
                            Nothing -> go (PQ.tail q) out
                            Just (Tuple arc es) -> go (Just $ addEvtsToQueue q es) (Cons arc out)
                else pure out

    go (Just queue) Nil
