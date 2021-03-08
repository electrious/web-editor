module SmartHouse.HouseTracer where

import Prelude

import Control.Alternative (empty)
import Control.Monad.Reader (ask)
import Data.Array (fromFoldable)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (foldl, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), head, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Editor.Common.Lenses (_face, _mouseMove, _name, _parent, _point, _position)
import Editor.ObjectAdder (createObjectAdder, mkCandidatePoint)
import Editor.PanelAPIInterpreter (_finished)
import Editor.SceneEvent (SceneMouseMoveEvent)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, gateDyn, sampleDyn, step)
import FRP.Event (Event)
import FRP.Event.Extra (multicast, performEvent)
import Math.Angle (degreeVal)
import Math.Line (Line, _end, _start, distToLine, intersection, lineVec, linesAngle, mkLine, perpendicularLine, projPointWithLine)
import Model.Polygon (Polygon, newPolygon)
import Rendering.DynamicNode (dynamic_)
import Rendering.Node (Node, _env, fixNodeE, line, mesh, node)
import Three.Core.Face3 (normal)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (LineBasicMaterial, LineDashedMaterial, MeshBasicMaterial, mkLineBasicMaterial, mkLineDashedMaterial, mkMeshBasicMaterial)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (Vector3, addScaled, dist, mkVec3, toVec2, toVec3, vecZ)

newtype HouseTracerConf = HouseTracerConf {
    mouseMove :: Event SceneMouseMoveEvent,
    canEdit   :: Dynamic Boolean
    }

derive instance newtypeHouseTracerConf :: Newtype HouseTracerConf _
instance defaultHouseTracerConf :: Default HouseTracerConf where
    def = HouseTracerConf {
        mouseMove : empty,
        canEdit   : pure false
        }

_canEdit :: forall t a r. Newtype t { canEdit :: a | r } => Lens' t a
_canEdit = _Newtype <<< prop (SProxy :: SProxy "canEdit")

newtype TracerState = TracerState {
    tracedVerts :: List Vector3,    -- all traced points in reverse order
    firstVert   :: Maybe Vector3,
    finished    :: Boolean
    }

derive instance newtypeTracerState :: Newtype TracerState _
derive instance genericTracerState :: Generic TracerState _
instance defaultTracerState :: Default TracerState where
    def = TracerState {
        tracedVerts : Nil,
        firstVert   : Nothing,
        finished    : false
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
               then s # _finished .~ true
                      # _tracedVerts %~ Cons v
               else s # _tracedVerts %~ Cons v
    Nothing -> s # _tracedVerts %~ Cons v
                 # _firstVert   .~ Just v

lastVert :: TracerState -> Maybe Vector3
lastVert s = head $ s ^. _tracedVerts

lastLine :: TracerState -> Maybe (Line Vector3)
lastLine st = f $ st ^. _tracedVerts
    where f (v1:v2:_) = Just $ mkLine v1 v2
          f _         = Nothing

allLines :: TracerState -> List (Line Vector3)
allLines st = fst $ foldl f (Tuple Nil Nothing) $ st ^. _tracedVerts
    where f (Tuple r Nothing) v = Tuple r (Just v)
          f (Tuple r (Just lv)) v = Tuple (mkLine v lv : r) (Just v)

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

tempLineTo :: Maybe Vector3 -> TracerState -> Maybe (Line Vector3)
tempLineTo t s = mkLine <$> t <*> lastVert s

lineMat :: LineBasicMaterial
lineMat = unsafePerformEffect $ mkLineBasicMaterial 0xeeeeee 2.0

renderLine :: forall e. Line Vector3 -> Node e Unit
renderLine l = void $ line (def # _name .~ "vert-adder-line") vs lineMat
    where vs = [l ^. _start, l ^. _end]

renderMaybeLine :: forall e. Maybe (Line Vector3) -> Node e Unit
renderMaybeLine Nothing  = pure unit
renderMaybeLine (Just l) = renderLine l
--------------------------------------------------------


--------------------------------------------------------
-- helper lines
--------------------------------------------------------

-- calculate the perpendicular helper line to the last polygon line
perpHelperLine :: TracerState -> Maybe (Line Vector3)
perpHelperLine = lastLine >>> map (map toVec2 >>> perpendicularLine >>> map (flip toVec3 0.0))

-- check whether the temp line is close to the perpendicular line
-- enough to show the helper line
canShowPerpLine :: Maybe (Line Vector3) -> Maybe (Line Vector3) -> Boolean
canShowPerpLine (Just l1) (Just l2) = almostParallel l1 l2
canShowPerpLine _ _                 = false

-- check if two lines is almost parallel or not
almostParallel :: Line Vector3 -> Line Vector3 -> Boolean
almostParallel l1 l2 = let a = degreeVal (linesAngle l1 l2) in a < 10.0 || a > 170.0

-- check if a point is close to the vector of the specified line
pointCloseToLine :: Vector3 -> Line Vector3 -> Boolean
pointCloseToLine v l = almostParallel l nl
    where nl = mkLine v (l ^. _start)

-- find the most parallel edge in the polygon to the specified point
paraHelperLine :: TracerState -> Maybe Vector3 -> Maybe (Line Vector3)
paraHelperLine st Nothing  = Nothing
paraHelperLine st (Just p) = foldl f Nothing $ allLines st
    where f Nothing l    = if pointCloseToLine p l then Just (extendLine l) else Nothing
          f v@(Just _) _ = v

          extendLine l = let s = l ^. _start
                             v = lineVec l
                             ns = addScaled s v (-20.0)
                             ne = addScaled s v 20.0
                         in mkLine ns ne

-- render helper lines
helperLineMat :: LineDashedMaterial
helperLineMat = unsafePerformEffect $ mkLineDashedMaterial 0xe28743 2.0 1.0 3.0 1.0

renderHelperLine :: forall e. Line Vector3 -> Node e Unit
renderHelperLine l = void $ line (def # _name .~ "helper-line") vs helperLineMat
    where vs = [l ^. _start, l ^. _end]

renderMaybeHelperLine :: forall e. Maybe (Line Vector3) -> Node e Unit
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

        f (Just p) st perpL paraL = Just $ snapPoint p st perpL paraL
        f Nothing _ _ _ = Nothing

        spDyn = f <$> pDyn <*> stDyn <*> perpLineShowDyn <*> paraLineDyn
        
    -- render temp line
    dynamic_ $ renderMaybeLine <$> tempLineDyn

    -- render the perpendicular helper line
    dynamic_ $ renderMaybeHelperLine <$> perpLineShowDyn

    -- render the parallel helper line
    dynamic_ $ renderMaybeHelperLine <$> paraLineDyn

    pure $ spDyn

--------------------------------------------------------

--------------------------------------------------------
-- point snapping
--------------------------------------------------------
snapPoint :: Vector3 -> TracerState -> Maybe (Line Vector3) -> Maybe (Line Vector3) -> Vector3
snapPoint p st Nothing Nothing           = fromMaybe p $ snapToVert p st
snapPoint p st (Just l) Nothing          = fromMaybe p $ snapToLine p st l
snapPoint p st Nothing (Just l)          = fromMaybe p $ snapToLine p st l
snapPoint p st (Just perpL) (Just paraL) = fromMaybe p $ snapToCrossing p st perpL paraL

snapToVert :: Vector3 -> TracerState -> Maybe Vector3
snapToVert p st = fst $ foldl f (Tuple Nothing 0.0) $ st ^. _tracedVerts
    where f o@(Tuple Nothing _) v = let d = dist p v
                                    in if d < 0.5 then Tuple (Just v) d else o
          f o@(Tuple (Just _) od) v = let d = dist p v
                                      in if d < od then Tuple (Just v) d else o

snapToLine :: Vector3 -> TracerState -> Line Vector3 -> Maybe Vector3
snapToLine p st l = f <$> lastVert st
    where f v = if distToLine p l < 0.3
                then projPointWithLine v p l
                else p

snapToCrossing :: Vector3 -> TracerState -> Line Vector3 -> Line Vector3 -> Maybe Vector3
snapToCrossing p st perpL paraL = let v1 = snapToLine p st perpL
                                      v2 = snapToLine p st paraL
                                      v3 = toVec3 (intersection (toVec2 <$> perpL) (toVec2 <$> paraL)) (vecZ p)
                                      g (Tuple Nothing _) v = Tuple (Just v) (dist p v)
                                      g o@(Tuple (Just _) od) v = let nd = dist p v
                                                                  in if nd < od
                                                                     then Tuple (Just v) nd
                                                                     else o
                                  in fst $ foldl g (Tuple Nothing 0.0) $ compact [v1, v2, Just v3]
    
--------------------------------------------------------

vertAdder :: Dynamic TracerState -> Node HouseTracerConf (Event Vector3)
vertAdder stDyn = do
    e <- ask
    let parent = e ^. _parent
        conf   = e ^. _env

        canEdit  = conf ^. _canEdit
        mouseEvt = conf ^. _mouseMove

        -- get a candidate point
        getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            pure $ Just $ mkCandidatePoint np (normal $ evt ^. _face)

        candPntDyn = step Nothing $ performEvent $ getCandPoint <$> gateDyn canEdit mouseEvt
        candVecDyn = map (view _position) <$> candPntDyn

    newPDyn <- helperLines stDyn candVecDyn

    let newCandPDyn = updPos <$> candPntDyn <*> newPDyn
        
        updPos (Just cand) (Just p) = Just $ cand # _position .~ p
        updPos v Nothing = v
        updPos Nothing _ = Nothing

    -- render the object adder
    let opt = def # _name .~ "vert-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.2)
    addedPntEvt <- node opt $ createObjectAdder newCandPDyn canEdit
    
    pure $ view _position <$> addedPntEvt


traceHouse :: Node HouseTracerConf (Event (Polygon Vector3))
traceHouse = node (def # _name .~ "house-tracer") $
    fixNodeE \stEvt -> do
        let stDyn = step def stEvt
        -- render the current state
        dynamic_ $ renderState <$> stDyn

        -- render the vertex adder
        newVertEvt <- vertAdder stDyn

        let newStEvt = multicast $ sampleDyn stDyn $ addNewVert <$> newVertEvt
            polyEvt = multicast $ newPolygon <<< view _tracedVerts <$> filter (view _finished) newStEvt

        pure { input: newStEvt, output: polyEvt }
