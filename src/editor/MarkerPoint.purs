module Editor.MarkerPoint where

import Prelude

import Control.Alt ((<|>))
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (class Foldable)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_active, _enabled, _index, _isActive, _isDragging, _name, _position, _tapped)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, dynEvent, gateDyn)
import FRP.Event (Event, sampleOn_)
import FRP.Event.Extra (performEvent)
import Model.ActiveMode (ActiveMode(..), isActive)
import Rendering.Node (_visible, getParent, tapMesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (BufferGeometry, CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Math.Vector (class Vector, getVector, updateVector)
import UI.DraggableObject (DragObjCfg, createDraggableObject, incZ)
import Util (latestAnyEvtWith, latestAnyEvtWithIdx)


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
    enabled  :: Dynamic Boolean,
    modifier :: Modifier v
}

derive instance newtypeVertMarkerPoint :: Newtype (VertMarkerPoint i v) _
instance defaultVertMarkerPoint :: (Default i, Default v) => Default (VertMarkerPoint i v) where
    def = VertMarkerPoint {
        position : def,
        index    : def,
        active   : pure Active,
        enabled  : pure true,
        modifier : def
        }

_modifier :: forall t a r. Newtype t { modifier :: a | r } => Lens' t a
_modifier = _Newtype <<< prop (SProxy :: SProxy "modifier")

newtype VertMarker i v = VertMarker {
    tapped     :: Event (Tuple i v),
    position   :: Event (Tuple i v),
    isDragging :: Event Boolean,
    dragEndPos :: Event v
}

derive instance newtypeVertMarker :: Newtype (VertMarker i v) _

_dragEndPos :: forall t a r. Newtype t { dragEndPos :: a | r } => Lens' t a
_dragEndPos = _Newtype <<< prop (SProxy :: SProxy "dragEndPos")

instance nodeRenderableVertMarkerPoint :: Vector v => NodeRenderable e (VertMarkerPoint i v) (VertMarker i v) where
    render m = do
        let enabled = m ^. _enabled
            cfg = def # _isActive .~ (isActive <$> m ^. _active)
                      # _enabled  .~ enabled
                      # _position .~ (getVector $ m ^. _position)
            mod = m ^. _modifier <<< _modifierFunc
        dragObj <- createDraggableObject (cfg :: DragObjCfg BufferGeometry)
        let posEvt   = performEvent $ (mod <<< updateVector (m ^. _position)) <$> dragObj ^. _position
            dragging = dragObj ^. _isDragging
        pure $ VertMarker {
            tapped     : const (Tuple (m ^. _index) (m ^. _position)) <$> gateDyn enabled (dragObj ^. _tapped),
            position   : Tuple (m ^. _index) <$> posEvt,
            isDragging : dragging,
            dragEndPos : sampleOn_ posEvt $ filter not dragging
        }

-- | get vertex markers' active status event
getVertMarkerActiveStatus :: forall f i v. FunctorWithIndex i f => Foldable f => Dynamic (f (VertMarker i v)) -> Event (Maybe i)
getVertMarkerActiveStatus msDyn = statusForDragging <|> statusForNewMarker
    where g idx m = (if _ then Just idx else Nothing) <$> m ^. _isDragging
          statusForDragging  = latestAnyEvtWithIdx g msDyn
          statusForNewMarker = const Nothing <$> dynEvent msDyn

getVertMarkerDragging :: forall i v f. Functor f => Foldable f => Dynamic (f (VertMarker i v)) -> Event Boolean
getVertMarkerDragging = latestAnyEvtWith (view _isDragging)

-----------------------------------------------------------
-- | internal object for middle marker point data
newtype MidMarkerPoint i v = MidMarkerPoint {
    position :: v,
    index    :: i,
    vert1    :: v,
    vert2    :: v,
    active   :: Dynamic ActiveMode,
    enabled  :: Dynamic Boolean
}

derive instance newtypeMidMarkerPoint :: Newtype (MidMarkerPoint i v) _

_vert1 :: forall t a r. Newtype t { vert1 :: a | r } => Lens' t a
_vert1 = _Newtype <<< prop (SProxy :: SProxy "vert1")

_vert2 :: forall t a r. Newtype t { vert2 :: a | r } => Lens' t a
_vert2 = _Newtype <<< prop (SProxy :: SProxy "vert2")

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
                          # _position .~ pure (incZ $ getVector $ p ^. _position)
                          # _visible  .~ (isActive <$> p ^. _active)
                     ) midGeometry midMaterial
        
        pure $ MidMarker { tapped : const p <$> gateDyn (p ^. _enabled) (m ^. _tapped) }


