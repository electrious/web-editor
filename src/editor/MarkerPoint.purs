module Editor.MarkerPoint where

import Prelude

import Control.Alt ((<|>))
import Data.Default (class Default, def)
import Data.Foldable (class Foldable)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_active, _index, _isActive, _isDragging, _name, _position, _tapped)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, dynEvent, latestEvt)
import FRP.Event (Event, keepLatest)
import FRP.Event.Extra (anyEvt, performEvent)
import Model.ActiveMode (ActiveMode, fromBoolean, isActive)
import Rendering.Node (_visible, getParent, tapMesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (CircleGeometry, Geometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Math.Vector (class Vector, getVector, updateVector)
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
newtype VertMarkerPoint i v = VertMarkerPoint {
    position :: v,
    index    :: i,
    active   :: Dynamic ActiveMode,
    modifier :: Modifier v
}

derive instance newtypeVertMarkerPoint :: Newtype (VertMarkerPoint i v) _

_modifier :: forall t a r. Newtype t { modifier :: a | r } => Lens' t a
_modifier = _Newtype <<< prop (SProxy :: SProxy "modifier")

newtype VertMarker i v = VertMarker {
    tapped     :: Event i,
    position   :: Event v,
    isDragging :: Event Boolean
}

derive instance newtypeVertMarker :: Newtype (VertMarker i v) _

instance nodeRenderableVertMarkerPoint :: Vector v => NodeRenderable e (VertMarkerPoint i v) (VertMarker i v) where
    render m = do
        let cfg = def # _isActive .~ (isActive <$> m ^. _active)
                      # _position .~ (getVector $ m ^. _position)
            mod = m ^. _modifier <<< _modifierFunc
        dragObj <- createDraggableObject (cfg :: DragObjCfg Geometry)
        
        pure $ VertMarker {
            tapped     : const (m ^. _index) <$> dragObj ^. _tapped,
            position   : performEvent $ (mod <<< updateVector (m ^. _position)) <$> dragObj ^. _position,
            isDragging : dragObj ^. _isDragging
        }

-- create a vertex marker point
mkVertMarkerPoint :: forall i v. Eq i => Modifier v -> Dynamic ActiveMode -> Dynamic (Maybe i) -> Tuple v i -> VertMarkerPoint i v
mkVertMarkerPoint m active actMarker (Tuple pos idx) = VertMarkerPoint {
    position : pos,
    index    : idx,
    active   : f <$> active <*> actMarker,
    modifier : m
    }
    where f act Nothing       = act
          f act (Just actIdx) = act && fromBoolean (actIdx == idx)

-- | get vertex markers' active status event
getVertMarkerActiveStatus :: forall f i v. FunctorWithIndex i f => Foldable f => Dynamic (f (VertMarker i v)) -> Event (Maybe i)
getVertMarkerActiveStatus msDyn = statusForDragging <|> statusForNewMarker
    where g idx m = (if _ then Just idx else Nothing) <$> m ^. _isDragging
          statusForDragging  = latestEvt $ anyEvt <<< mapWithIndex g <$> msDyn
          statusForNewMarker = const Nothing <$> dynEvent msDyn

getVertMarkerDragging :: forall i v f. Functor f => Foldable f => Dynamic (f (VertMarker i v)) -> Event Boolean
getVertMarkerDragging ms = latestEvt $ foldEvtWith (view _isDragging) <$> ms


-----------------------------------------------------------
-- | internal object for middle marker point data
newtype MidMarkerPoint i v = MidMarkerPoint {
    position :: v,
    index    :: i,
    active   :: Dynamic ActiveMode
}

derive instance newtypeMidMarkerPoint :: Newtype (MidMarkerPoint i v) _

newtype MidMarker i v = MidMarker {
    tapped :: Event (MidMarkerPoint i v)
}

derive instance newtypeMidMarker :: Newtype (MidMarker i v) _

-- | create material and geometry for the middle marker.
midMaterial :: MeshBasicMaterial
midMaterial = unsafePerformEffect (mkMeshBasicMaterial 0x22ff22)

midGeometry :: CircleGeometry
midGeometry = unsafePerformEffect (mkCircleGeometry 0.3 32)

instance nodeRenderableMidMarkerPoint :: Vector v => NodeRenderable e (MidMarkerPoint i v) (MidMarker i v) where
    render p = do
        parent <- getParent
        m <- tapMesh (def # _name     .~ "mid-marker"
                          # _position .~ pure (getVector $ p ^. _position)
                          # _visible  .~ (isActive <$> p ^. _active)
                     ) midGeometry midMaterial
        
        pure $ MidMarker { tapped : const p <$> m ^. _tapped }

-- | given a list of vertices position, calculate all middle points
midMarkerPoints :: forall i v f. Vector v => Functor f => Dynamic ActiveMode -> f (Tuple i v) -> f (MidMarkerPoint i v)
midMarkerPoints active = map mkPoint
    where mkPoint (Tuple idx v) = MidMarkerPoint {
              position : v,
              index    : idx,
              active   : active
              }

