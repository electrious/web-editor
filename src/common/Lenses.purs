module Editor.Common.Lenses where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))

_leadId :: forall t a r. Newtype t { leadId :: a | r } => Lens' t a
_leadId = _Newtype <<< prop (SProxy :: SProxy "leadId")

_houseId :: forall t a r. Newtype t { houseId :: a | r } => Lens' t a
_houseId = _Newtype <<< prop (SProxy :: SProxy "houseId")

_id :: forall t a r. Newtype t { id :: a | r } => Lens' t a
_id = _Newtype <<< prop (SProxy :: SProxy "id")

_roofId :: forall t a r. Newtype t { roofId :: a | r } => Lens' t a
_roofId = _Newtype <<< prop (SProxy :: SProxy "roofId")

_roofs :: forall t a r. Newtype t { roofs :: a | r } => Lens' t a
_roofs = _Newtype <<< prop (SProxy :: SProxy "roofs")

_center :: forall t a r. Newtype t { center :: a | r } => Lens' t a
_center = _Newtype <<< prop (SProxy :: SProxy "center")

_normal :: forall t a r. Newtype t { normal :: a | r } => Lens' t a
_normal = _Newtype <<< prop (SProxy :: SProxy "normal")

_slope :: forall t a r. Newtype t { slope :: a | r } => Lens' t a
_slope = _Newtype <<< prop (SProxy :: SProxy "slope")

_polygon :: forall t a r. Newtype t { polygon :: a | r } => Lens' t a
_polygon = _Newtype <<< prop (SProxy :: SProxy "polygon")

_orientation :: forall t a r. Newtype t { orientation :: a | r } => Lens' t a
_orientation = _Newtype <<< prop (SProxy :: SProxy "orientation")

_alignment :: forall t a r. Newtype t { alignment :: a | r } => Lens' t a
_alignment = _Newtype <<< prop (SProxy :: SProxy "alignment")

_x :: forall t a r. Newtype t { x :: a | r } => Lens' t a
_x = _Newtype <<< prop (SProxy :: SProxy "x")

_y :: forall t a r. Newtype t { y :: a | r } => Lens' t a
_y = _Newtype <<< prop (SProxy :: SProxy "y")

_z :: forall t a r. Newtype t { z :: a | r } => Lens' t a
_z = _Newtype <<< prop (SProxy :: SProxy "z")

_deltaX :: forall t a r. Newtype t { deltaX :: a | r } => Lens' t a
_deltaX = _Newtype <<< prop (SProxy :: SProxy "deltaX")

_deltaY :: forall t a r. Newtype t { deltaY :: a | r } => Lens' t a
_deltaY = _Newtype <<< prop (SProxy :: SProxy "deltaY")

_width :: forall t a r. Newtype t { width :: a | r } => Lens' t a
_width = _Newtype <<< prop (SProxy :: SProxy "width")

_height :: forall t a r. Newtype t { height :: a | r } => Lens' t a
_height = _Newtype <<< prop (SProxy :: SProxy "height")

_disposable :: forall t a r. Newtype t { disposable :: a | r } => Lens' t a
_disposable = _Newtype <<< prop (SProxy :: SProxy "disposable")

_mesh :: forall t a r. Newtype t { mesh :: a | r } => Lens' t a
_mesh = _Newtype <<< prop (SProxy :: SProxy "mesh")

_tapped :: forall t a r. Newtype t { tapped :: a | r } => Lens' t a
_tapped = _Newtype <<< prop (SProxy :: SProxy "tapped")

_dragged :: forall t a r. Newtype t { dragged :: a | r } => Lens' t a
_dragged = _Newtype <<< prop (SProxy :: SProxy "dragged")

_dragType :: forall t a r. Newtype t { dragType :: a | r } => Lens' t a
_dragType = _Newtype <<< prop (SProxy :: SProxy "dragType")

_dragDelta :: forall t a r. Newtype t { dragDelta :: a | r } => Lens' t a
_dragDelta = _Newtype <<< prop (SProxy :: SProxy "dragDelta")

_position :: forall t a r. Newtype t { position :: a | r } => Lens' t a
_position = _Newtype <<< prop (SProxy :: SProxy "position")

_point :: forall t a r. Newtype t { point :: a | r } => Lens' t a
_point = _Newtype <<< prop (SProxy :: SProxy "point")

_canDrag :: forall t a r. Newtype t { canDrag :: a | r } => Lens' t a
_canDrag = _Newtype <<< prop (SProxy :: SProxy "canDrag")

_isDragging :: forall t a r. Newtype t { isDragging :: a | r } => Lens' t a
_isDragging = _Newtype <<< prop (SProxy :: SProxy "isDragging")

_lastDragEvt :: forall t a r. Newtype t { lastDragEvt :: a | r } => Lens' t a
_lastDragEvt = _Newtype <<< prop (SProxy :: SProxy "lastDragEvt")

_curDragEvt :: forall t a r. Newtype t { curDragEvt :: a | r } => Lens' t a
_curDragEvt = _Newtype <<< prop (SProxy :: SProxy "curDragEvt")

_zoomed :: forall t a r. Newtype t { zoomed :: a | r } => Lens' t a 
_zoomed = _Newtype <<< prop (SProxy :: SProxy "zoomed")

_shiftDragged :: forall t a r. Newtype t { shiftDragged :: a | r } => Lens' t a
_shiftDragged = _Newtype <<< prop (SProxy :: SProxy "shiftDragged")

_mouseMove :: forall t a r. Newtype t { mouseMove :: a | r } => Lens' t a
_mouseMove = _Newtype <<< prop (SProxy :: SProxy "mouseMove")

_domPosition :: forall t a r. Newtype t { domPosition :: a | r } => Lens' t a
_domPosition = _Newtype <<< prop (SProxy :: SProxy "domPosition")

_distance :: forall t a r. Newtype t { distance :: a | r } => Lens' t a
_distance = _Newtype <<< prop (SProxy :: SProxy "distance")

_face :: forall t a r. Newtype t { face :: a | r } => Lens' t a 
_face = _Newtype <<< prop (SProxy :: SProxy "face")

_index :: forall t a r. Newtype t { index :: a | r } => Lens' t a
_index = _Newtype <<< prop (SProxy :: SProxy "index")

_wrapper :: forall t a r. Newtype t { wrapper :: a | r } => Lens' t a
_wrapper = _Newtype <<< prop (SProxy :: SProxy "wrapper")

_geometry :: forall t a r. Newtype t { geometry :: a | r } => Lens' t a
_geometry = _Newtype <<< prop (SProxy :: SProxy "geometry")

_verticeTree :: forall t a r. Newtype t { verticeTree :: a | r } => Lens' t a
_verticeTree = _Newtype <<< prop (SProxy :: SProxy "verticeTree")

_textureInfo :: forall t a r. Newtype t { textureInfo :: a | r } => Lens' t a
_textureInfo = _Newtype <<< prop (SProxy :: SProxy "textureInfo")

_panelType :: forall t a r. Newtype t { panelType :: a | r } => Lens' t a
_panelType = _Newtype <<< prop (SProxy :: SProxy "panelType")

_rackingType :: forall t a r. Newtype t { rackingType :: a | r } => Lens' t a
_rackingType = _Newtype <<< prop (SProxy :: SProxy "rackingType")

_mountSpacing :: forall t a r. Newtype t { mountSpacing :: a | r } => Lens' t a
_mountSpacing = _Newtype <<< prop (SProxy :: SProxy "mountSpacing")

_rafterSpacing :: forall t a r. Newtype t { rafterSpacing :: a | r } => Lens' t a
_rafterSpacing = _Newtype <<< prop (SProxy :: SProxy "rafterSpacing")

_roofRackings :: forall t a r. Newtype t { roofRackings :: a | r } => Lens' t a
_roofRackings = _Newtype <<< prop (SProxy :: SProxy "roofRackings")

_modeDyn :: forall t a r. Newtype t { modeDyn :: a | r } => Lens' t a
_modeDyn = _Newtype <<< prop (SProxy :: SProxy "modeDyn")
