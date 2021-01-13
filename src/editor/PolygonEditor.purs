module Editor.PolygonEditor where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Custom.Mesh (TappableMesh)
import Data.Array (length, mapWithIndex, range, zip)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Foldable (class Foldable)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_index, _isDragging, _name, _polygon, _position, _tapped)
import Editor.SceneEvent (SceneTapEvent)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, dynEvent, step)
import FRP.Event (Event, keepLatest, sampleOn)
import FRP.Event.Extra (anyEvt, mergeArray, multicast, performEvent, skip)
import Model.Hardware.PanelModel (_isActive)
import Model.Polygon (class PolyVertex, Polygon, _polyVerts, addVertexAt, delVertexAt, getPos, newPolygon, polyCenter, polyMidPoints, updatePos)
import Rendering.DynamicNode (renderEvent)
import Rendering.Node (Node, _visible, fixNodeE, fixNodeEWith, getParent, tapMesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (CircleGeometry, Geometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import UI.DraggableObject (DragObjCfg, createDraggableObject)
import Util (foldEvtWith)

-- a data type to modify vectors
newtype Modifier v = Modifier (v -> Effect v)

derive instance newtypeModifier :: Newtype (Modifier v) _
instance defaultModifier :: Default (Modifier v) where
    def = Modifier pure

_modifierFunc :: forall v. Lens' (Modifier v) (v -> Effect v)
_modifierFunc = _Newtype

-----------------------------------------------------------
-- vertex marker
newtype VertMarkerPoint v = VertMarkerPoint {
    position :: v,
    index    :: Int,
    isActive :: Dynamic Boolean,
    modifier :: Modifier v
}

derive instance newtypeVertMarkerPoint :: Newtype (VertMarkerPoint v) _

_modifier :: forall t a r. Newtype t { modifier :: a | r } => Lens' t a
_modifier = _Newtype <<< prop (SProxy :: SProxy "modifier")

newtype VertMarker v = VertMarker {
    tapped     :: Event Int,
    position   :: Event v,
    isDragging :: Event Boolean
}

derive instance newtypeVertMarker :: Newtype (VertMarker v) _

instance nodeRenderableVertMarkerPoint :: PolyVertex v => NodeRenderable e (VertMarkerPoint v) (VertMarker v) where
    render m = do
        let cfg = def # _isActive .~ m ^. _isActive
                      # _position .~ getPos (m ^. _position)
            mod = m ^. _modifier <<< _modifierFunc
        dragObj <- createDraggableObject (cfg :: DragObjCfg Geometry)
        
        pure $ VertMarker {
            tapped     : const (m ^. _index) <$> dragObj ^. _tapped,
            position   : performEvent $ (mod <<< updatePos (m ^. _position)) <$> dragObj ^. _position,
            isDragging : dragObj ^. _isDragging
        }

-- create a vertex marker point
mkVertMarkerPoint :: forall v. Modifier v -> Event Boolean -> Event (Maybe Int) -> Tuple v Int -> VertMarkerPoint v
mkVertMarkerPoint m polyActive actMarker (Tuple pos idx) = VertMarkerPoint {
                                                               position : pos,
                                                               index    : idx,
                                                               isActive : step false isActive,
                                                               modifier : m
                                                               }
    where f act Nothing       = act
          f act (Just actIdx) = act && actIdx == idx

          isActive = multicast $ f <$> polyActive <*> actMarker

-- create vertex markers for an array of vertices
mkVertMarkerPoints :: forall v. Modifier v -> Event Boolean -> Event (Maybe Int) -> Polygon v -> Array (VertMarkerPoint v)
mkVertMarkerPoints m polyActive actMarker poly = mkVertMarkerPoint m polyActive actMarker <$> zip ps (range 0 (length ps - 1))
    where ps = poly ^. _polyVerts

-- | get vertex markers' active status event
getVertMarkerActiveStatus :: forall v. Event (Array (VertMarker v)) -> Event (Maybe Int)
getVertMarkerActiveStatus ms = statusForDragging <|> statusForNewMarker
    where g idx m = (if _ then Just idx else Nothing) <$> m ^. _isDragging
          statusForDragging  = keepLatest $ (anyEvt <<< mapWithIndex g) <$> ms
          statusForNewMarker = const Nothing <$> ms

getVertMarkerDragging :: forall v. Event (Array (VertMarker v)) -> Event Boolean
getVertMarkerDragging ms = keepLatest $ (foldEvtWith (view _isDragging)) <$> ms

-- create new vertex markers
setupVertMarkers :: forall e v. PolyVertex v => Modifier v -> Event Boolean -> Event (Maybe Int) -> Event (Polygon v) -> Node e (Event (Array (VertMarker v)))
setupVertMarkers m polyActive activeMarker polyEvt = renderEvent vertMarkers
    where vertMarkers = mkVertMarkerPoints m polyActive activeMarker <$> polyEvt

-----------------------------------------------------------
-- Marker to delete the current polygon
polyDelMat :: MeshBasicMaterial
polyDelMat = unsafePerformEffect (mkMeshBasicMaterial 0xffaa22)

polyDelGeo :: CircleGeometry
polyDelGeo = unsafePerformEffect (mkCircleGeometry 0.6 32)

-- | create the polygon delete marker button
mkPolyDelMarker :: forall e v. PolyVertex v => Event v -> Event Boolean -> Node e TappableMesh
mkPolyDelMarker posEvt visEvt = tapMesh (def # _name     .~ "delete-marker"
                                             # _position .~ step def (getPos <$> posEvt)
                                             # _visible  .~ step false visEvt
                                        ) polyDelGeo polyDelMat

-----------------------------------------------------------
-- | internal object for middle marker point data
newtype MidMarkerPoint v = MidMarkerPoint {
    position :: v,
    index    :: Int,
    isActive :: Dynamic Boolean
}

derive instance newtypeMidMarkerPoint :: Newtype (MidMarkerPoint v) _

newtype MidMarker v = MidMarker {
    tapped :: Event (MidMarkerPoint v)
}

derive instance newtypeMidMarker :: Newtype (MidMarker v) _

-- | create material and geometry for the middle marker.
midMaterial :: MeshBasicMaterial
midMaterial = unsafePerformEffect (mkMeshBasicMaterial 0x22ff22)

midGeometry :: CircleGeometry
midGeometry = unsafePerformEffect (mkCircleGeometry 0.3 32)

instance nodeRenderableMidMarkerPoint :: PolyVertex v => NodeRenderable e (MidMarkerPoint v) (MidMarker v) where
    render p = do
        parent <- getParent
        m <- tapMesh (def # _name     .~ "mid-marker"
                          # _position .~ pure (getPos $ p ^. _position)
                          # _visible  .~ (p ^. _isActive)
                     ) midGeometry midMaterial
        
        pure $ MidMarker { tapped : const p <$> m ^. _tapped }

-- | given a list of vertices position, calculate all middle points
midMarkerPoints :: forall v. PolyVertex v => Dynamic Boolean -> Polygon v -> Array (MidMarkerPoint v)
midMarkerPoints active poly = mkPoint <$> polyMidPoints poly
    where mkPoint (Tuple idx v) = MidMarkerPoint {
              position : v,
              index    : idx,
              isActive : active
              }


-- | render all middle markers
mkMidMarkers :: forall e v. PolyVertex v => Event Boolean -> Event (Polygon v) -> Node e (Event (MidMarkerPoint v))
mkMidMarkers active polyEvt = do
    let actDyn     = step false active
        mPointsEvt = midMarkerPoints actDyn <$> polyEvt
    markers :: (Event (Array (MidMarker v))) <- renderEvent mPointsEvt
    pure $ keepLatest $ getTapEvt <$> markers

newtype PolyEditorConf v = PolyEditorConf {
    isActive     :: Dynamic Boolean,
    polygon      :: Polygon v,
    vertModifier :: Modifier v
    }

derive instance newtypePolyEditorConf :: Newtype (PolyEditorConf v) _
instance defaultPolyEditorConf :: Default (PolyEditorConf v) where
    def = PolyEditorConf {
        isActive     : pure false,
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
getPosition :: forall v. Array (VertMarker v) -> Event (Polygon v)
getPosition os = newPolygon <$> mergeArray (f <$> os)
    where f o = o ^. _position

-- | merge all tapped events in a foldable list of objects support it.
getTapEvt :: forall t a r f. Functor f => Foldable f => Newtype t { tapped :: Event a | r } => f t -> Event a
getTapEvt = anyEvt <<< map (view _tapped)

-- | create polygon editor
createPolyEditor :: forall e v. PolyVertex v => PolyEditorConf v -> Node e (PolyEditor v)
createPolyEditor cfg = do
    let poly   = cfg ^. _polygon
        active = cfg ^. _isActive
    
    fixNodeEWith false \polyActive ->
        fixNodeEWith poly \polyEvt ->
            fixNodeE \actMarkerEvt -> do
                -- pipe the 'active' param event into internal polyActive event
                vertMarkersEvt <- multicast <$> setupVertMarkers (cfg ^. _vertModifier) polyActive actMarkerEvt polyEvt

                -- event for active vertex marker
                let newActMarkerEvt = getVertMarkerActiveStatus vertMarkersEvt

                    -- get new positions after dragging
                    vertsAfterDrag = keepLatest $ getPosition <$> vertMarkersEvt

                    -- merge new vertices after dragging and vertices after adding/deleting
                    newPolyEvt = multicast $ polyEvt <|> vertsAfterDrag
    
                    midActive = multicast $ lift2 (\pa am -> pa && am == Nothing) polyActive actMarkerEvt
                -- create mid markers for adding new vertices
                toAddEvt <- mkMidMarkers midActive newPolyEvt

                let addVert p ply = addVertexAt (p ^. _index) (p ^. _position) ply
                    vertsAfterAdd = compact (sampleOn newPolyEvt $ addVert <$> toAddEvt)

                    -- get delete event of tapping on a marker
                    delEvts = keepLatest $ getTapEvt <$> vertMarkersEvt
                    -- calculate new vertices after deleting a vertex
                    vertsAfterDel = sampleOn newPolyEvt (delVertexAt <$> delEvts)
    
                    -- update the real vertex list after adding/deleting
                    polygonEvt = multicast $ vertsAfterAdd <|> vertsAfterDel

                    polyActEvt = dynEvent active <|> (const true <$> polygonEvt)
                -- create the polygon delete button
                polyDel <- mkPolyDelMarker (polyCenter <$> newPolyEvt) polyActive

                let editor = PolyEditor {
                    polygon    : skip 1 newPolyEvt,  -- skip the default polygon rendered
                    delete     : multicast $ polyDel ^. _tapped,
                    isDragging : getVertMarkerDragging vertMarkersEvt
                    }
                pure { input: newActMarkerEvt, output: { input: polygonEvt, output : { input: polyActEvt, output: editor } } }
