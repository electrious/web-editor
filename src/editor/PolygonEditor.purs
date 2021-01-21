module Editor.PolygonEditor where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Custom.Mesh (TappableMesh)
import Data.Array (length, range, zip)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Foldable (class Foldable)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_active, _index, _name, _polygon, _position, _tapped)
import Editor.MarkerPoint (MidMarker, MidMarkerPoint, Modifier, VertMarker, VertMarkerPoint, getVertMarkerActiveStatus, getVertMarkerDragging, midMarkerPoints, mkVertMarkerPoint)
import Editor.SceneEvent (SceneTapEvent)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, current, dynEvent, latestEvt, step)
import FRP.Event (Event, keepLatest, sampleOn)
import FRP.Event.Extra (anyEvt, mergeArray, multicast, skip)
import Model.ActiveMode (ActiveMode(..), fromBoolean, isActive)
import Model.Polygon (Polygon, _polyVerts, addVertexAt, delVertexAt, newPolygon, polyCenter, polyMidPoints)
import Rendering.DynamicNode (renderDynamic, renderEvent)
import Rendering.Node (Node, _visible, fixNodeDWith, tapMesh)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Math.Vector (class Vector, getVector)

-- create vertex markers for an array of vertices
mkVertMarkerPoints :: forall v. Modifier v -> Dynamic ActiveMode -> Dynamic (Maybe Int) -> Polygon v -> Array (VertMarkerPoint Int v)
mkVertMarkerPoints m polyActive actMarker poly = mkVertMarkerPoint m polyActive actMarker <$> zip ps (range 0 (length ps - 1))
    where ps = poly ^. _polyVerts

-- create new vertex markers
setupVertMarkers :: forall e v. Vector v => Modifier v -> Dynamic ActiveMode -> Dynamic (Maybe Int) -> Dynamic (Polygon v) -> Node e (Dynamic (Array (VertMarker Int v)))
setupVertMarkers m polyActive activeMarker polyEvt = renderDynamic $ mkVertMarkerPoints m polyActive activeMarker <$> polyEvt

-----------------------------------------------------------
-- Marker to delete the current polygon
polyDelMat :: MeshBasicMaterial
polyDelMat = unsafePerformEffect (mkMeshBasicMaterial 0xffaa22)

polyDelGeo :: CircleGeometry
polyDelGeo = unsafePerformEffect (mkCircleGeometry 0.6 32)

-- | create the polygon delete marker button
mkPolyDelMarker :: forall e v. Vector v => Event v -> Dynamic ActiveMode -> Node e TappableMesh
mkPolyDelMarker posEvt actDyn = tapMesh (def # _name     .~ "delete-marker"
                                             # _position .~ step def (getVector <$> posEvt)
                                             # _visible  .~ (isActive <$> actDyn)
                                        ) polyDelGeo polyDelMat


-- | render all middle markers
setupMidMarkers :: forall e v. Vector v => Dynamic ActiveMode -> Event (Polygon v) -> Node e (Event (MidMarkerPoint Int v))
setupMidMarkers actDyn polyEvt = do
    let mPointsEvt = midMarkerPoints actDyn <<< polyMidPoints <$> polyEvt
    markers :: (Event (Array (MidMarker Int v))) <- renderEvent mPointsEvt
    pure $ keepLatest $ getTapEvt <$> markers

newtype PolyEditorConf v = PolyEditorConf {
    active       :: Dynamic ActiveMode,
    polygon      :: Polygon v,
    vertModifier :: Modifier v
    }

derive instance newtypePolyEditorConf :: Newtype (PolyEditorConf v) _
instance defaultPolyEditorConf :: Default (PolyEditorConf v) where
    def = PolyEditorConf {
        active       : pure Inactive,
        polygon      : def,
        vertModifier : def
        }

_vertModifier :: forall t a r. Newtype t { vertModifier :: a | r } => Lens' t a
_vertModifier = _Newtype <<< prop (SProxy :: SProxy "vertModifier")

newtype PolyEditor v = PolyEditor {
    polygon    :: Event (Polygon v),
    delete     :: Event SceneTapEvent,
    isDragging :: Event Boolean
}

derive instance newtypePolyEditor :: Newtype (PolyEditor v) _

_delete :: forall t a r. Newtype t { delete :: a | r } => Lens' t a
_delete = _Newtype <<< prop (SProxy :: SProxy "delete")

-- get new positions after dragging
getPosition :: forall i v. Array (VertMarker i v) -> Event (Polygon v)
getPosition os = newPolygon <$> mergeArray (f <$> os)
    where f o = o ^. _position

-- | merge all tapped events in a foldable list of objects support it.
getTapEvt :: forall t a r f. Functor f => Foldable f => Newtype t { tapped :: Event a | r } => f t -> Event a
getTapEvt = anyEvt <<< map (view _tapped)

-- | create polygon editor
createPolyEditor :: forall e v. Default v => Vector v => PolyEditorConf v -> Node e (PolyEditor v)
createPolyEditor cfg = do
    let poly   = cfg ^. _polygon
        active = cfg ^. _active

    defAct <- liftEffect $ current active
    
    fixNodeDWith defAct \polyActive ->
        fixNodeDWith poly \polyDyn ->
            fixNodeDWith Nothing \actMarkerDyn -> do
                -- pipe the 'active' param event into internal polyActive event
                vertMarkersDyn <- setupVertMarkers (cfg ^. _vertModifier) polyActive actMarkerDyn polyDyn

                -- event for active vertex marker
                let newActMarkerEvt = getVertMarkerActiveStatus vertMarkersDyn

                    -- get new positions after dragging
                    vertsAfterDrag = latestEvt $ getPosition <$> vertMarkersDyn

                    -- merge new vertices after dragging and vertices after adding/deleting
                    newPolyEvt = multicast $ dynEvent polyDyn <|> vertsAfterDrag
    
                    midActive = lift2 (\pa am -> pa && fromBoolean (am == Nothing)) polyActive actMarkerDyn
                -- create mid markers for adding new vertices
                toAddEvt <- setupMidMarkers midActive newPolyEvt

                let addVert p ply = addVertexAt (p ^. _index) (p ^. _position) ply
                    vertsAfterAdd = compact (sampleOn newPolyEvt $ addVert <$> toAddEvt)

                    -- get delete event of tapping on a marker
                    delEvts = latestEvt $ getTapEvt <$> vertMarkersDyn
                    -- calculate new vertices after deleting a vertex
                    vertsAfterDel = sampleOn newPolyEvt (delVertexAt <$> delEvts)
    
                    -- update the real vertex list after adding/deleting
                    polygonEvt = multicast $ vertsAfterAdd <|> vertsAfterDel

                    polyActEvt = dynEvent active <|> (const Active <$> polygonEvt)
                -- create the polygon delete button
                polyDel <- mkPolyDelMarker (polyCenter <$> newPolyEvt) polyActive

                let editor = PolyEditor {
                    polygon    : skip 1 newPolyEvt,  -- skip the default polygon rendered
                    delete     : multicast $ polyDel ^. _tapped,
                    isDragging : multicast $ getVertMarkerDragging vertMarkersDyn
                    }
                pure { input: newActMarkerEvt, output: { input: polygonEvt, output : { input: polyActEvt, output: editor } } }
