module Editor.PolygonEditor where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Taihe.Mesh (TappableMesh)
import Data.Array (length, range, zip)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Data.Tuple (Tuple(..), fst)
import Editor.Common.Lenses (_active, _index, _name, _polygon, _position, _tapped)
import Editor.MarkerPoint (MidMarker, MidMarkerPoint(..), Modifier, VertMarker, VertMarkerPoint, _modifier, getVertMarkerActiveStatus, getVertMarkerDragging)
import Taihe.SceneEvent (SceneTapEvent)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, current, dynEvent, sampleDyn)
import FRP.Event (Event)
import FRP.Event.Extra (multicast)
import Model.ActiveMode (ActiveMode(..), fromBoolean, isActive)
import Model.Polygon (Polygon, _polyVerts, addVertexAt, delVertexAt, polyCenter, polyMidPoints, updateVertAt)
import Rendering.DynamicNode (renderDynamic)
import Rendering.Node (Node, _raycastable, _visible, fixNodeDWith, tapMesh)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Math.Vector (class Vector, getVector, incX)
import Util (latestAnyEvtWith)

-- create vertex markers for an array of vertices
mkVertMarkerPoints :: forall v. Default v => Modifier v -> Dynamic ActiveMode -> Dynamic (Maybe Int) -> Polygon v -> Array (VertMarkerPoint Int v)
mkVertMarkerPoints m polyActive actMarker poly = mkV <$> zip ps (range 0 (length ps - 1))
    where ps = poly ^. _polyVerts
          mkV (Tuple pos idx) = def # _position .~ pos
                                    # _index    .~ idx
                                    # _active   .~ (f idx <$> polyActive <*> actMarker)
                                    # _modifier .~ m
          f _   act Nothing       = act
          f idx act (Just actIdx) = act && fromBoolean (actIdx == idx)

-- create new vertex markers
setupVertMarkers :: forall e v. Default v => Vector v => Modifier v -> Dynamic ActiveMode -> Dynamic (Maybe Int) -> Dynamic (Polygon v) -> Node e (Dynamic (Array (VertMarker Int v)))
setupVertMarkers m polyActive activeMarker polyEvt = renderDynamic $ mkVertMarkerPoints m polyActive activeMarker <$> polyEvt

-----------------------------------------------------------
-- Marker to delete the current polygon
polyDelMat :: MeshBasicMaterial
polyDelMat = unsafePerformEffect (mkMeshBasicMaterial 0xffaa22)

polyDelGeo :: CircleGeometry
polyDelGeo = unsafePerformEffect (mkCircleGeometry 0.6 32)

-- | create the polygon delete marker button
mkPolyDelMarker :: forall e v. Vector v => Dynamic v -> Dynamic ActiveMode -> Node e TappableMesh
mkPolyDelMarker posDyn actDyn = do
    let act = isActive <$> actDyn
    tapMesh (def # _name     .~ "delete-marker"
                 # _position .~ (getVector <$> posDyn)
                 # _visible  .~ act
                 # _raycastable .~ act
            ) polyDelGeo polyDelMat

polyFinMat :: MeshBasicMaterial
polyFinMat = unsafePerformEffect $ mkMeshBasicMaterial 0x22ff22

-- | create a finish marker button
mkPolyFinMarker :: forall e v. Vector v => Dynamic v -> Dynamic ActiveMode -> Node e TappableMesh
mkPolyFinMarker posDyn actDyn = do
    let act = isActive <$> actDyn
    tapMesh (def # _name     .~ "finish-marker"
                 # _position .~ (getVector <$> posDyn)
                 # _visible  .~ act
                 # _raycastable .~ act
            ) polyDelGeo polyFinMat

-- | given a list of vertices position, calculate all middle points
midMarkerPoints :: forall v f. Vector v => Functor f => Dynamic ActiveMode -> f (Tuple Int v) -> f (MidMarkerPoint Int v)
midMarkerPoints active = map mkPoint
    where mkPoint (Tuple idx v) = MidMarkerPoint {
              position : v,
              index    : idx,
              vert1    : v,  -- init these two values with the provided v, it's useless in PolygonEditor
              vert2    : v,
              active   : active,
              enabled  : pure true
              }

-- | render all middle markers
setupMidMarkers :: forall e v. Vector v => Dynamic ActiveMode -> Dynamic (Polygon v) -> Node e (Event (MidMarkerPoint Int v))
setupMidMarkers actDyn polyDyn = do
    let mPointsDyn = midMarkerPoints actDyn <<< polyMidPoints <$> polyDyn
    markers :: (Dynamic (Array (MidMarker Int v))) <- renderDynamic mPointsDyn
    pure $ latestAnyEvtWith (view _tapped) markers

newtype PolyEditorConf v = PolyEditorConf {
    active       :: Dynamic ActiveMode,
    polygon      :: Polygon v,
    vertModifier :: Modifier v,
    showFinish   :: Boolean
    }

derive instance newtypePolyEditorConf :: Newtype (PolyEditorConf v) _
instance defaultPolyEditorConf :: Default (PolyEditorConf v) where
    def = PolyEditorConf {
        active       : pure Inactive,
        polygon      : def,
        vertModifier : def,
        showFinish   : false
        }

_vertModifier :: forall t a r. Newtype t { vertModifier :: a | r } => Lens' t a
_vertModifier = _Newtype <<< prop (Proxy :: Proxy "vertModifier")

_showFinish :: forall t a r. Newtype t { showFinish :: a | r } => Lens' t a
_showFinish = _Newtype <<< prop (Proxy :: Proxy "showFinish")

newtype PolyEditor v = PolyEditor {
    polygon    :: Event (Polygon v),
    delete     :: Event SceneTapEvent,
    finished   :: Event Unit,
    isDragging :: Event Boolean
}

derive instance newtypePolyEditor :: Newtype (PolyEditor v) _

_delete :: forall t a r. Newtype t { delete :: a | r } => Lens' t a
_delete = _Newtype <<< prop (Proxy :: Proxy "delete")

updateVertex :: forall v. Tuple Int v -> Polygon v -> Polygon v
updateVertex (Tuple i v) p = updateVertAt i v p

-- | create polygon editor
createPolyEditor :: forall e v. Default v => Vector v => PolyEditorConf v -> Node e (PolyEditor v)
createPolyEditor cfg = do
    let poly   = cfg ^. _polygon
        active = cfg ^. _active

    defAct <- liftEffect $ current active
    
    fixNodeDWith defAct \polyActive ->
        fixNodeDWith poly \polyDyn ->
            fixNodeDWith poly \noDragPolyDyn ->
                fixNodeDWith Nothing \actMarkerDyn -> do
                    -- pipe the 'active' param event into internal polyActive event
                    vertMarkersDyn <- setupVertMarkers (cfg ^. _vertModifier) polyActive actMarkerDyn noDragPolyDyn

                    -- event for active vertex marker
                    let newActMarkerEvt = getVertMarkerActiveStatus vertMarkersDyn

                        -- get new positions after dragging
                        vertDragged = latestAnyEvtWith (view _position) vertMarkersDyn
                        vertsAfterDrag = sampleDyn polyDyn $ updateVertex <$> vertDragged
                    
                        midActive = lift2 (\pa am -> pa && fromBoolean (am == Nothing)) polyActive actMarkerDyn
                    -- create mid markers for adding new vertices
                    toAddEvt <- setupMidMarkers midActive polyDyn

                    let addVert p ply = addVertexAt (p ^. _index) (p ^. _position) ply
                        vertsAfterAdd = compact (sampleDyn polyDyn $ addVert <$> toAddEvt)

                        -- get delete event of tapping on a marker
                        delEvts = map fst $ latestAnyEvtWith (view _tapped) vertMarkersDyn
                        -- calculate new vertices after deleting a vertex
                        vertsAfterDel = sampleDyn polyDyn (delVertexAt <$> delEvts)
    
                        -- update the real vertex list after adding/deleting
                        polygonEvt = multicast $ vertsAfterAdd <|> vertsAfterDel

                        -- new polygon after all kinds of changes
                        allPolyEvt = multicast $ polygonEvt <|> vertsAfterDrag

                        polyActEvt = dynEvent active <|> (const Active <$> polygonEvt)

                        centerPosDyn = polyCenter <$> polyDyn
                        finBtnPosDyn = incX 3.0 <$> centerPosDyn
                    -- create the polygon delete button
                    polyDel <- mkPolyDelMarker centerPosDyn polyActive
                    polyFin <- mkPolyFinMarker finBtnPosDyn polyActive

                    let editor = PolyEditor {
                            polygon    : allPolyEvt,  -- skip the default polygon rendered
                            delete     : multicast $ polyDel ^. _tapped,
                            finished   : multicast $ const unit <$> polyFin ^. _tapped,
                            isDragging : multicast $ getVertMarkerDragging vertMarkersDyn
                            }
                    pure { input: newActMarkerEvt, output: { input: polygonEvt, output : { input: allPolyEvt, output : { input: polyActEvt, output: editor } } } }
