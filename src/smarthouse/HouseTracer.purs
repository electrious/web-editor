module SmartHouse.HouseTracer where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Monad.Reader (ask)
import Data.Array (fromFoldable)
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (class Foldable, foldl, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), head, (:))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Editor.Common.Lenses (_face, _modeDyn, _mouseMove, _name, _parent, _point, _position)
import Editor.ObjectAdder (AdderType(..), createObjectAdder, mkCandidatePoint)
import Editor.SceneEvent (SceneMouseMoveEvent)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, sampleDyn, step)
import FRP.Event (Event, withLast)
import FRP.Event.Extra (delay, multicast, performEvent)
import Math.Angle (degreeVal)
import Math.LineSeg (LineSeg, _end, _start, distToLineSeg, intersection, lineVec, linesAngle, mkLineSeg, perpendicularLineSeg, projPointWithLineSeg)
import Model.ActiveMode (ActiveMode(..), isActive)
import Model.Polygon (Polygon, newPolygon)
import Rendering.DynamicNode (dynamic_)
import Rendering.Node (Node, _visible, dashLine, fixNodeE, line, mesh, node)
import Three.Core.Face3 (normal)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (LineBasicMaterial, LineDashedMaterial, MeshBasicMaterial, mkLineBasicMaterial, mkLineDashedMaterial, mkMeshBasicMaterial)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (Vector3, addScaled, dist, mkVec3, toVec2, toVec3)

newtype HouseTracerConf = HouseTracerConf {
    modeDyn     :: Dynamic ActiveMode,
    mouseMove   :: Event SceneMouseMoveEvent,
    undoTracing :: Event Unit,
    stopTracing :: Event Unit
    }

derive instance newtypeHouseTracerConf :: Newtype HouseTracerConf _
instance defaultHouseTracerConf :: Default HouseTracerConf where
    def = HouseTracerConf {
        modeDyn     : pure Active,
        mouseMove   : empty,
        undoTracing : empty,
        stopTracing : empty
        }

_undoTracing :: forall t a r. Newtype t { undoTracing :: a | r } => Lens' t a
_undoTracing = _Newtype <<< prop (SProxy :: SProxy "undoTracing")

_stopTracing :: forall t a r. Newtype t { stopTracing :: a | r } => Lens' t a
_stopTracing = _Newtype <<< prop (SProxy :: SProxy "stopTracing")

data TracerMode = Waiting  -- waiting user to start tracing
                | Tracing

derive instance genericTracerMode :: Generic TracerMode _
derive instance eqTracerMode :: Eq TracerMode
instance showTracerMode :: Show TracerMode where
    show = genericShow

fromBoolean :: Boolean -> TracerMode
fromBoolean true  = Waiting
fromBoolean false = Tracing

-- result data type of the house tracer
newtype TracerResult = TracerResult {
    tracedPolygon :: Event (Polygon Vector3),
    tracerMode    :: Event TracerMode,
    canUndo       :: Event Boolean
    }

derive instance newtypeTracerResult :: Newtype TracerResult _
instance defaultTracerResult :: Default TracerResult where
    def = TracerResult {
        tracedPolygon : empty,
        tracerMode    : empty,
        canUndo       : empty
        }

_tracedPolygon :: forall t a r. Newtype t { tracedPolygon :: a | r } => Lens' t a
_tracedPolygon = _Newtype <<< prop (SProxy :: SProxy "tracedPolygon")

_tracerMode :: forall t a r. Newtype t { tracerMode :: a | r } => Lens' t a
_tracerMode = _Newtype <<< prop (SProxy :: SProxy "tracerMode")

_canUndo :: forall t a r. Newtype t { canUndo :: a | r } => Lens' t a
_canUndo = _Newtype <<< prop (SProxy :: SProxy "canUndo")

-- internal state data
newtype TracerState = TracerState {
    tracedVerts :: List Vector3,    -- all traced points in reverse order
    firstVert   :: Maybe Vector3,
    tracerMode  :: TracerMode
    }

derive instance newtypeTracerState :: Newtype TracerState _
derive instance genericTracerState :: Generic TracerState _
instance defaultTracerState :: Default TracerState where
    def = TracerState {
        tracedVerts : Nil,
        firstVert   : Nothing,
        tracerMode  : Waiting
        }
instance showTracerState :: Show TracerState where
    show = genericShow

_tracedVerts :: forall t a r. Newtype t { tracedVerts :: a | r } => Lens' t a
_tracedVerts = _Newtype <<< prop (SProxy :: SProxy "tracedVerts")

_firstVert :: forall t a r. Newtype t { firstVert :: a | r } => Lens' t a
_firstVert = _Newtype <<< prop (SProxy :: SProxy "firstVert")

addNewVert :: Vector3 -> TracerState -> TracerState
addNewVert v s = case s ^. _firstVert of
    Just fv -> if dist v fv < 0.2
               then s # _tracerMode  .~ Waiting
                      # _tracedVerts %~ Cons fv
               else s # _tracedVerts %~ Cons v
    Nothing -> s # _tracedVerts %~ Cons v
                 # _firstVert   .~ Just v
                 # _tracerMode  .~ Tracing


canUndo :: TracerState -> Boolean
canUndo = isJust <<< head <<< view _tracedVerts

undo :: TracerState -> TracerState
undo s = case s ^. _tracedVerts of
    Nil     -> s
    (v:Nil) -> def
    (v:ls)  -> s # _tracedVerts .~ ls


resetTracer :: Unit -> TracerState -> TracerState
resetTracer _ _ = def

lastVert :: TracerState -> Maybe Vector3
lastVert s = head $ s ^. _tracedVerts

lastLine :: TracerState -> Maybe (LineSeg Vector3)
lastLine st = f $ st ^. _tracedVerts
    where f (v1:v2:_) = Just $ mkLineSeg v1 v2
          f _         = Nothing

allLines :: TracerState -> List (LineSeg Vector3)
allLines st = fst $ foldl f (Tuple Nil Nothing) $ st ^. _tracedVerts
    where f (Tuple r Nothing) v = Tuple r (Just v)
          f (Tuple r (Just lv)) v = Tuple (mkLineSeg v lv : r) (Just v)

--------------------------------------------------------
-- render the tracer state
--------------------------------------------------------

vertMat :: MeshBasicMaterial
vertMat = unsafePerformEffect $ mkMeshBasicMaterial 0x00ee00

vertGeo :: CircleGeometry
vertGeo = unsafePerformEffect $ mkCircleGeometry 0.2 16

renderVert :: forall e. Vector3 -> Node e Unit
renderVert p = void $ mesh (def # _name     .~ "vertex"
                                # _position .~ pure p) vertGeo vertMat

renderState :: forall e. TracerState -> Node e Unit
renderState st = do
    let vs = st ^. _tracedVerts
    traverse_ renderVert vs
    void $ line (def # _name .~ "polygon-line") (fromFoldable vs) lineMat

--------------------------------------------------------
-- temp lines
--------------------------------------------------------

tempLineTo :: Maybe Vector3 -> TracerState -> Maybe (LineSeg Vector3)
tempLineTo t s = mkLineSeg <$> t <*> lastVert s

lineMat :: LineBasicMaterial
lineMat = unsafePerformEffect $ mkLineBasicMaterial 0xeeeeee 4.0

renderLine :: forall e. LineSeg Vector3 -> Node e Unit
renderLine l = void $ line (def # _name .~ "vert-adder-line") vs lineMat
    where vs = [l ^. _start, l ^. _end]

renderLineWith :: forall e. LineSeg Vector3 -> LineBasicMaterial -> Node e Unit
renderLineWith l mat = void $ line (def # _name .~ "vert-adder-line") vs mat
    where vs = [l ^. _start, l ^. _end]                       

renderMaybeLine :: forall e. Maybe (LineSeg Vector3) -> Node e Unit
renderMaybeLine Nothing  = pure unit
renderMaybeLine (Just l) = renderLine l
--------------------------------------------------------


--------------------------------------------------------
-- helper lines
--------------------------------------------------------

-- calculate the perpendicular helper line to the last polygon line
perpHelperLine :: TracerState -> Maybe (LineSeg Vector3)
perpHelperLine = lastLine >>> map (map toVec2 >>> perpendicularLineSeg >>> map (flip toVec3 0.0))

-- check whether the temp line is close to the perpendicular line
-- enough to show the helper line
canShowPerpLine :: Maybe (LineSeg Vector3) -> Maybe (LineSeg Vector3) -> Boolean
canShowPerpLine (Just l1) (Just l2) = almostParallel l1 l2
canShowPerpLine _ _                 = false

-- check if two lines is almost parallel or not
almostParallel :: LineSeg Vector3 -> LineSeg Vector3 -> Boolean
almostParallel l1 l2 = let a = degreeVal (linesAngle l1 l2) in a < 10.0 || a > 170.0

-- check if a point is close to the vector of the specified line
pointCloseToLine :: Vector3 -> LineSeg Vector3 -> Boolean
pointCloseToLine v l = almostParallel l nl
    where nl = mkLineSeg v (l ^. _start)

-- find the most parallel edge in the polygon to the specified point
paraHelperLine :: TracerState -> Maybe Vector3 -> Maybe (LineSeg Vector3)
paraHelperLine st Nothing  = Nothing
paraHelperLine st (Just p) = foldl f Nothing $ allLines st
    where f Nothing l    = if pointCloseToLine p l then Just (extendLine l) else Nothing
          f v@(Just _) _ = v

          extendLine l = let s = l ^. _start
                             v = lineVec l
                             ns = addScaled s v (-20.0)
                             ne = addScaled s v 20.0
                         in mkLineSeg ns ne

-- helper line to connect to the first point traced
endHelperLine :: TracerState -> Maybe Vector3 -> Maybe (LineSeg Vector3)
endHelperLine st Nothing  = Nothing
endHelperLine st (Just p) = foldl f Nothing $ allLines st
    where tempL = mkLineSeg <$> (st ^. _firstVert) <*> Just p
          f Nothing l    = if fromMaybe false (almostParallel <$> Just l <*> tempL) then mkL l <$> st ^. _firstVert else Nothing
          f v@(Just _) _ = v

          mkL l s = let v = lineVec l
                        ns = addScaled s v (-20.0)
                        ne = addScaled s v 20.0
                    in mkLineSeg ns ne

-- render helper lines
helperLineMat :: LineDashedMaterial
helperLineMat = unsafePerformEffect $ mkLineDashedMaterial 0xe28743 4.0 1.0 3.0 2.0

renderHelperLine :: forall e. LineSeg Vector3 -> Node e Unit
renderHelperLine l = void $ dashLine (def # _name .~ "helper-line") vs helperLineMat
    where vs = [l ^. _start, l ^. _end]

renderMaybeHelperLine :: forall e. Maybe (LineSeg Vector3) -> Node e Unit
renderMaybeHelperLine Nothing  = pure unit
renderMaybeHelperLine (Just l) = renderHelperLine l

helperLines :: forall e. Dynamic TracerState -> Dynamic (Maybe Vector3) -> Node e (Dynamic (Maybe Vector3))
helperLines stDyn pDyn = do
    let tempLineDyn = tempLineTo <$> pDyn <*> stDyn
        -- calculate the perpendicular helper line to render
        perpLineDyn = perpHelperLine <$> stDyn

        procPerpLine t p = if canShowPerpLine t p then p else Nothing
        perpLineShowDyn = procPerpLine <$> tempLineDyn <*> perpLineDyn

        -- parallel helper lines
        paraLineDyn = paraHelperLine <$> stDyn <*> pDyn

        -- end helper line
        endLineDyn = endHelperLine <$> stDyn <*> pDyn

        f (Just p) st perpL paraL endL = snapPoint p st perpL paraL endL
        f Nothing _ _ _ _ = Nothing

        spDyn = f <$> pDyn <*> stDyn <*> perpLineShowDyn <*> paraLineDyn <*> endLineDyn
        
    -- render temp line
    dynamic_ $ renderMaybeLine <$> tempLineDyn

    -- render the perpendicular helper line
    dynamic_ $ renderMaybeHelperLine <$> perpLineShowDyn

    -- render the parallel helper line
    dynamic_ $ renderMaybeHelperLine <$> paraLineDyn

    -- render the end helper line
    dynamic_ $ renderMaybeHelperLine <$> endLineDyn

    pure $ spDyn

--------------------------------------------------------

--------------------------------------------------------
-- point snapping
--------------------------------------------------------
snapPoint :: Vector3 -> TracerState -> Maybe (LineSeg Vector3) -> Maybe (LineSeg Vector3) -> Maybe (LineSeg Vector3) -> Maybe Vector3
snapPoint p st Nothing Nothing Nothing          = closestPoint p $ st ^. _tracedVerts
snapPoint p st (Just l) Nothing Nothing         = snapToLine p st l
snapPoint p st Nothing (Just l) Nothing         = snapToLine p st l
snapPoint p st Nothing Nothing (Just l)         = snapToLine p st l
snapPoint p st (Just perpL) Nothing (Just endL) = closestPoint p $ addToL (intersection perpL endL) (st ^. _tracedVerts)
snapPoint p st Nothing (Just paraL) (Just endL) = closestPoint p $ addToL (intersection paraL endL) (st ^. _tracedVerts)
snapPoint p st (Just perpL) (Just paraL) _      = closestPoint p $ addToL (intersection perpL paraL) (st ^. _tracedVerts)

addToL :: forall a. Maybe a -> List a -> List a
addToL (Just v) l = Cons v l
addToL Nothing l  = l

closestPoint :: forall f. Foldable f => Vector3 -> f Vector3 -> Maybe Vector3
closestPoint p = fst <<< foldl f (Tuple Nothing 0.0)
    where f o@(Tuple Nothing _) v = let d = dist p v
                                    in if d < 0.5 then Tuple (Just v) d else o
          f o@(Tuple (Just _) od) v = let d = dist p v
                                      in if d < od then Tuple (Just v) d else o

snapToLine :: Vector3 -> TracerState -> LineSeg Vector3 -> Maybe Vector3
snapToLine p st l = f <$> lastVert st
    where f v = if distToLineSeg p l < 0.5
                then projPointWithLineSeg v p l
                else p

--------------------------------------------------------

vertAdder :: forall e. HouseTracerConf -> Dynamic TracerState -> Node e (Event Vector3)
vertAdder conf stDyn = do
    e <- ask
    let parent = e ^. _parent
        mouseEvt = conf ^. _mouseMove

        -- get a candidate point
        getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            pure $ Just $ mkCandidatePoint np (normal $ evt ^. _face)

        candPntDyn = step Nothing $ performEvent $ getCandPoint <$> mouseEvt
        candVecDyn = map (view _position) <$> candPntDyn

    newPDyn <- helperLines stDyn candVecDyn

    let newCandPDyn = updPos <$> candPntDyn <*> newPDyn
        
        updPos (Just cand) (Just p) = Just $ cand # _position .~ p
        updPos v Nothing = v
        updPos Nothing _ = Nothing

    -- render the object adder
    let opt = def # _name .~ "vert-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.01)
    addedPntEvt <- node opt $ createObjectAdder CrossAdder newCandPDyn (isActive <$> conf ^. _modeDyn)
    
    pure $ view _position <$> addedPntEvt


traceHouse :: forall e. HouseTracerConf -> Node e TracerResult
traceHouse conf = node (def # _name    .~ "house-tracer"
                            # _visible .~ (isActive <$> conf ^. _modeDyn)) $
    fixNodeE \stEvt -> do
        let stDyn = step def stEvt
        -- render the current state
        dynamic_ $ renderState <$> stDyn

        -- render the vertex adder
        newVertEvt <- vertAdder conf stDyn

        let newStEvt = multicast $ sampleDyn stDyn $ (addNewVert <$> newVertEvt) <|>
                                                     (const undo <$> conf ^. _undoTracing) <|>
                                                     (resetTracer <$> conf ^. _stopTracing)

            modeEvt = multicast $ view _tracerMode <$> stEvt

            -- only fire the new polygon event when the mode turns from Tracing to Waiting
            f { last : l, now : n } = case l of
                Just ls -> ls ^. _tracerMode == Tracing && n ^. _tracerMode == Waiting
                Nothing -> false
            g { now: n } = n ^. _tracedVerts
            
            polyEvt  = multicast $ newPolygon <<< g <$> filter f (withLast newStEvt)

            -- reset state after finish tracing a new house
            newStAfterFinishEvt = delay 20 $ const def <$> polyEvt

            res = def # _tracedPolygon .~ polyEvt
                      # _tracerMode    .~ modeEvt
                      # _canUndo       .~ (canUndo <$> stEvt)

        pure { input: multicast (newStAfterFinishEvt <|> newStEvt), output: res }
