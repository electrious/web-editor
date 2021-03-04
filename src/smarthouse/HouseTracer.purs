module SmartHouse.HouseTracer where

import Prelude

import Control.Alternative (empty)
import Control.Monad.Reader (ask)
import Data.Array (fromFoldable)
import Data.Default (class Default, def)
import Data.Foldable (traverse_)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), head, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_face, _mouseMove, _name, _parent, _point, _position)
import Editor.ObjectAdder (createObjectAdder, mkCandidatePoint)
import Editor.RoofEditor (toVec2)
import Editor.SceneEvent (SceneMouseMoveEvent)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, gateDyn, sampleDyn, step)
import FRP.Event (Event)
import FRP.Event.Extra (performEvent)
import Math.Angle (degreeVal)
import Math.Line (Line, _end, _start, linesAngle, mkLine, perpendicularLine)
import Rendering.DynamicNode (dynamic_)
import Rendering.Node (Node, _env, fixNodeE, line, mesh, node)
import Three.Core.Face3 (normal)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (LineBasicMaterial, LineDashedMaterial, MeshBasicMaterial, mkLineBasicMaterial, mkLineDashedMaterial, mkMeshBasicMaterial)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (Vector3, mkVec3, toVec3)

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
    tracedVerts :: List Vector3    -- all traced points in reverse order
    }

derive instance newtypeTracerState :: Newtype TracerState _
instance defaultTracerState :: Default TracerState where
    def = TracerState {
        tracedVerts : Nil
        }

_tracedVerts :: forall t a r. Newtype t { tracedVerts :: a | r } => Lens' t a
_tracedVerts = _Newtype <<< prop (SProxy :: SProxy "tracedVerts")

addNewVert :: Vector3 -> TracerState -> TracerState
addNewVert v s = s # _tracedVerts %~ Cons v

lastVert :: TracerState -> Maybe Vector3
lastVert s = head $ s ^. _tracedVerts

lastLine :: TracerState -> Maybe (Line Vector3)
lastLine st = f $ st ^. _tracedVerts
    where f (v1:v2:_) = Just $ mkLine v1 v2
          f _         = Nothing

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
canShowPerpLine (Just l1) (Just l2) = degreeVal (linesAngle l1 l2) < 10.0
canShowPerpLine _ _                 = false


-- render helper lines
helperLineMat :: LineDashedMaterial
helperLineMat = unsafePerformEffect $ mkLineDashedMaterial 0xe28743 2.0 1.0 3.0 1.0

renderHelperLine :: forall e. Line Vector3 -> Node e Unit
renderHelperLine l = void $ line (def # _name .~ "helper-line") vs helperLineMat
    where vs = [l ^. _start, l ^. _end]

renderMaybeHelperLine :: forall e. Maybe (Line Vector3) -> Node e Unit
renderMaybeHelperLine Nothing  = pure unit
renderMaybeHelperLine (Just l) = renderHelperLine l


helperLines :: forall e. Dynamic TracerState -> Dynamic (Maybe Vector3) -> Node e Unit
helperLines stDyn pDyn = do
    let tempLineDyn = tempLineTo <$> pDyn <*> stDyn
        -- calculate the perpendicular helper line to render
        perpLineDyn = perpHelperLine <$> stDyn

        procPerpLine t p = if canShowPerpLine t p then  p else Nothing
        showPerpDyn = procPerpLine <$> tempLineDyn <*> perpLineDyn
        
    -- render temp line
    dynamic_ $ renderMaybeLine <$> tempLineDyn

    -- render the perpendicular helper line
    dynamic_ $ renderMaybeHelperLine <$> perpLineDyn

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

    helperLines stDyn candVecDyn
    
    -- render the object adder
    let opt = def # _name .~ "vert-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.2)
    addedPntEvt <- node opt $ createObjectAdder candPntDyn canEdit
    
    pure $ view _position <$> addedPntEvt


traceHouse :: Node HouseTracerConf (Event Unit)
traceHouse = node (def # _name .~ "house-tracer") $
    fixNodeE \stEvt -> do
        let stDyn = step def stEvt
        -- render the current state
        dynamic_ $ renderState <$> stDyn

        -- render the vertex adder
        newVertEvt <- vertAdder stDyn

        let newStEvt = sampleDyn stDyn $ addNewVert <$> newVertEvt

        pure { input: newStEvt, output: empty }
