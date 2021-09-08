module Editor.Common.Lenses where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))

_leadId :: forall t a r. Newtype t { leadId :: a | r } => Lens' t a
_leadId = _Newtype <<< prop (Proxy :: Proxy "leadId")

_houseId :: forall t a r. Newtype t { houseId :: a | r } => Lens' t a
_houseId = _Newtype <<< prop (Proxy :: Proxy "houseId")

_id :: forall t a r. Newtype t { id :: a | r } => Lens' t a
_id = _Newtype <<< prop (Proxy :: Proxy "id")

_roofId :: forall t a r. Newtype t { roofId :: a | r } => Lens' t a
_roofId = _Newtype <<< prop (Proxy :: Proxy "roofId")

_roofs :: forall t a r. Newtype t { roofs :: a | r } => Lens' t a
_roofs = _Newtype <<< prop (Proxy :: Proxy "roofs")

_roof :: forall t a r. Newtype t { roof :: a | r } => Lens' t a
_roof = _Newtype <<< prop (Proxy :: Proxy "roof")

_center :: forall t a r. Newtype t { center :: a | r } => Lens' t a
_center = _Newtype <<< prop (Proxy :: Proxy "center")

_normal :: forall t a r. Newtype t { normal :: a | r } => Lens' t a
_normal = _Newtype <<< prop (Proxy :: Proxy "normal")

_slope :: forall t a r. Newtype t { slope :: a | r } => Lens' t a
_slope = _Newtype <<< prop (Proxy :: Proxy "slope")

_polygon :: forall t a r. Newtype t { polygon :: a | r } => Lens' t a
_polygon = _Newtype <<< prop (Proxy :: Proxy "polygon")

_orientation :: forall t a r. Newtype t { orientation :: a | r } => Lens' t a
_orientation = _Newtype <<< prop (Proxy :: Proxy "orientation")

_alignment :: forall t a r. Newtype t { alignment :: a | r } => Lens' t a
_alignment = _Newtype <<< prop (Proxy :: Proxy "alignment")

_x :: forall t a r. Newtype t { x :: a | r } => Lens' t a
_x = _Newtype <<< prop (Proxy :: Proxy "x")

_y :: forall t a r. Newtype t { y :: a | r } => Lens' t a
_y = _Newtype <<< prop (Proxy :: Proxy "y")

_z :: forall t a r. Newtype t { z :: a | r } => Lens' t a
_z = _Newtype <<< prop (Proxy :: Proxy "z")

_length :: forall t a r. Newtype t { length :: a | r } => Lens' t a
_length = _Newtype <<< prop (Proxy :: Proxy "length")

_deltaX :: forall t a r. Newtype t { deltaX :: a | r } => Lens' t a
_deltaX = _Newtype <<< prop (Proxy :: Proxy "deltaX")

_deltaY :: forall t a r. Newtype t { deltaY :: a | r } => Lens' t a
_deltaY = _Newtype <<< prop (Proxy :: Proxy "deltaY")

_centerX :: forall t a r. Newtype t { centerX :: a | r } => Lens' t a
_centerX = _Newtype <<< prop (Proxy :: Proxy "centerX")

_centerY :: forall t a r. Newtype t { centerY :: a | r } => Lens' t a
_centerY = _Newtype <<< prop (Proxy :: Proxy "centerY")

_width :: forall t a r. Newtype t { width :: a | r } => Lens' t a
_width = _Newtype <<< prop (Proxy :: Proxy "width")

_height :: forall t a r. Newtype t { height :: a | r } => Lens' t a
_height = _Newtype <<< prop (Proxy :: Proxy "height")

_object :: forall t a r. Newtype t { object :: a | r } => Lens' t a
_object = _Newtype <<< prop (Proxy :: Proxy "object")

_disposable :: forall t a r. Newtype t { disposable :: a | r } => Lens' t a
_disposable = _Newtype <<< prop (Proxy :: Proxy "disposable")

_mesh :: forall t a r. Newtype t { mesh :: a | r } => Lens' t a
_mesh = _Newtype <<< prop (Proxy :: Proxy "mesh")

_tapped :: forall t a r. Newtype t { tapped :: a | r } => Lens' t a
_tapped = _Newtype <<< prop (Proxy :: Proxy "tapped")

_dragged :: forall t a r. Newtype t { dragged :: a | r } => Lens' t a
_dragged = _Newtype <<< prop (Proxy :: Proxy "dragged")

_dragType :: forall t a r. Newtype t { dragType :: a | r } => Lens' t a
_dragType = _Newtype <<< prop (Proxy :: Proxy "dragType")

_dragDelta :: forall t a r. Newtype t { dragDelta :: a | r } => Lens' t a
_dragDelta = _Newtype <<< prop (Proxy :: Proxy "dragDelta")

_position :: forall t a r. Newtype t { position :: a | r } => Lens' t a
_position = _Newtype <<< prop (Proxy :: Proxy "position")

_rotation :: forall t a r. Newtype t { rotation :: a | r } => Lens' t a
_rotation = _Newtype <<< prop (Proxy :: Proxy "rotation")

_scale :: forall t a r. Newtype t { scale :: a | r } => Lens' t a
_scale = _Newtype <<< prop (Proxy :: Proxy "scale")

_point :: forall t a r. Newtype t { point :: a | r } => Lens' t a
_point = _Newtype <<< prop (Proxy :: Proxy "point")

_canDrag :: forall t a r. Newtype t { canDrag :: a | r } => Lens' t a
_canDrag = _Newtype <<< prop (Proxy :: Proxy "canDrag")

_dragging :: forall t a r. Newtype t { dragging :: a | r } => Lens' t a
_dragging = _Newtype <<< prop (Proxy :: Proxy "dragging")

_isDragging :: forall t a r. Newtype t { isDragging :: a | r } => Lens' t a
_isDragging = _Newtype <<< prop (Proxy :: Proxy "isDragging")

_lastDragEvt :: forall t a r. Newtype t { lastDragEvt :: a | r } => Lens' t a
_lastDragEvt = _Newtype <<< prop (Proxy :: Proxy "lastDragEvt")

_curDragEvt :: forall t a r. Newtype t { curDragEvt :: a | r } => Lens' t a
_curDragEvt = _Newtype <<< prop (Proxy :: Proxy "curDragEvt")

_zoomed :: forall t a r. Newtype t { zoomed :: a | r } => Lens' t a 
_zoomed = _Newtype <<< prop (Proxy :: Proxy "zoomed")

_shiftDragged :: forall t a r. Newtype t { shiftDragged :: a | r } => Lens' t a
_shiftDragged = _Newtype <<< prop (Proxy :: Proxy "shiftDragged")

_mouseMove :: forall t a r. Newtype t { mouseMove :: a | r } => Lens' t a
_mouseMove = _Newtype <<< prop (Proxy :: Proxy "mouseMove")

_domPosition :: forall t a r. Newtype t { domPosition :: a | r } => Lens' t a
_domPosition = _Newtype <<< prop (Proxy :: Proxy "domPosition")

_distance :: forall t a r. Newtype t { distance :: a | r } => Lens' t a
_distance = _Newtype <<< prop (Proxy :: Proxy "distance")

_face :: forall t a r. Newtype t { face :: a | r } => Lens' t a 
_face = _Newtype <<< prop (Proxy :: Proxy "face")

_index :: forall t a r. Newtype t { index :: a | r } => Lens' t a
_index = _Newtype <<< prop (Proxy :: Proxy "index")

_indices :: forall t a r. Newtype t { indices :: a | r } => Lens' t a
_indices = _Newtype <<< prop (Proxy :: Proxy "indices")

_wrapper :: forall t a r. Newtype t { wrapper :: a | r } => Lens' t a
_wrapper = _Newtype <<< prop (Proxy :: Proxy "wrapper")

_geometry :: forall t a r. Newtype t { geometry :: a | r } => Lens' t a
_geometry = _Newtype <<< prop (Proxy :: Proxy "geometry")

_verticeTree :: forall t a r. Newtype t { verticeTree :: a | r } => Lens' t a
_verticeTree = _Newtype <<< prop (Proxy :: Proxy "verticeTree")

_textureInfo :: forall t a r. Newtype t { textureInfo :: a | r } => Lens' t a
_textureInfo = _Newtype <<< prop (Proxy :: Proxy "textureInfo")

_panelType :: forall t a r. Newtype t { panelType :: a | r } => Lens' t a
_panelType = _Newtype <<< prop (Proxy :: Proxy "panelType")

_rackingType :: forall t a r. Newtype t { rackingType :: a | r } => Lens' t a
_rackingType = _Newtype <<< prop (Proxy :: Proxy "rackingType")

_mountSpacing :: forall t a r. Newtype t { mountSpacing :: a | r } => Lens' t a
_mountSpacing = _Newtype <<< prop (Proxy :: Proxy "mountSpacing")

_rafterSpacing :: forall t a r. Newtype t { rafterSpacing :: a | r } => Lens' t a
_rafterSpacing = _Newtype <<< prop (Proxy :: Proxy "rafterSpacing")

_roofRackings :: forall t a r. Newtype t { roofRackings :: a | r } => Lens' t a
_roofRackings = _Newtype <<< prop (Proxy :: Proxy "roofRackings")

_modeDyn :: forall t a r. Newtype t { modeDyn :: a | r } => Lens' t a
_modeDyn = _Newtype <<< prop (Proxy :: Proxy "modeDyn")

_arrayNumber :: forall t a r. Newtype t { arrayNumber :: a | r } => Lens' t a
_arrayNumber = _Newtype <<< prop (Proxy :: Proxy "arrayNumber")

_flashes :: forall t a r. Newtype t { flashes :: a | r } => Lens' t a
_flashes = _Newtype <<< prop (Proxy :: Proxy "flashes")

_rails :: forall t a r. Newtype t { rails :: a | r } => Lens' t a
_rails = _Newtype <<< prop (Proxy :: Proxy "rails")

_railsNum :: forall t a r. Newtype t { railsNum :: a | r } => Lens' t a
_railsNum = _Newtype <<< prop (Proxy :: Proxy "railsNum")

_splices :: forall t a r. Newtype t { splices :: a | r } => Lens' t a 
_splices = _Newtype <<< prop (Proxy :: Proxy "splices")

_lfeet :: forall t a r. Newtype t { lfeet :: a | r } => Lens' t a 
_lfeet = _Newtype <<< prop (Proxy :: Proxy "lfeet")

_clamps :: forall t a r. Newtype t { clamps :: a | r } => Lens' t a
_clamps = _Newtype <<< prop (Proxy :: Proxy "clamps")

_stoppers :: forall t a r. Newtype t { stoppers :: a | r } => Lens' t a 
_stoppers = _Newtype <<< prop (Proxy :: Proxy "stoppers")

_mounts :: forall t a r. Newtype t { mounts :: a | r } => Lens' t a
_mounts = _Newtype <<< prop (Proxy :: Proxy "mounts")

_bridges :: forall t a r. Newtype t { bridges :: a | r } => Lens' t a
_bridges = _Newtype <<< prop (Proxy :: Proxy "bridges")

_skirts :: forall t a r. Newtype t { skirts :: a | r } => Lens' t a
_skirts = _Newtype <<< prop (Proxy :: Proxy "skirts")

_leftEndCaps :: forall t a r. Newtype t { leftEndCaps :: a | r } => Lens' t a
_leftEndCaps = _Newtype <<< prop (Proxy :: Proxy "leftEndCaps")

_rightEndCaps :: forall t a r. Newtype t { rightEndCaps :: a | r } => Lens' t a
_rightEndCaps = _Newtype <<< prop (Proxy :: Proxy "rightEndCaps")

_supportRails :: forall t a r. Newtype t { supportRails :: a | r } => Lens' t a
_supportRails = _Newtype <<< prop (Proxy :: Proxy "supportRails")

_baseMounts :: forall t a r. Newtype t { baseMounts :: a | r } => Lens' t a
_baseMounts = _Newtype <<< prop (Proxy :: Proxy "baseMounts")

_tiltLegs :: forall t a r. Newtype t { tiltLegs :: a | r } => Lens' t a 
_tiltLegs = _Newtype <<< prop (Proxy :: Proxy "tiltLegs")

_chassis :: forall t a r. Newtype t { chassis :: a | r } => Lens' t a
_chassis = _Newtype <<< prop (Proxy :: Proxy "chassis")

_blocks :: forall t a r. Newtype t { blocks :: a | r } => Lens' t a
_blocks = _Newtype <<< prop (Proxy :: Proxy "blocks")

_hoods :: forall t a r. Newtype t { hoods :: a | r } => Lens' t a
_hoods = _Newtype <<< prop (Proxy :: Proxy "hoods")

_item :: forall t a r. Newtype t { item :: a | r } => Lens' t a
_item = _Newtype <<< prop (Proxy :: Proxy "item")

_minX :: forall t a r. Newtype t { minX :: a | r } => Lens' t a
_minX = _Newtype <<< prop (Proxy :: Proxy "minX")

_minY :: forall t a r. Newtype t { minY :: a | r } => Lens' t a
_minY = _Newtype <<< prop (Proxy :: Proxy "minY")

_maxX :: forall t a r. Newtype t { maxX :: a | r } => Lens' t a
_maxX = _Newtype <<< prop (Proxy :: Proxy "maxX")

_maxY :: forall t a r. Newtype t { maxY :: a | r } => Lens' t a
_maxY = _Newtype <<< prop (Proxy :: Proxy "maxY")

_panels :: forall t a r. Newtype t { panels :: a | r } => Lens' t a
_panels = _Newtype <<< prop (Proxy :: Proxy "panels")

_rowNumber :: forall t a r. Newtype t { rowNumber :: a | r } => Lens' t a
_rowNumber = _Newtype <<< prop (Proxy :: Proxy "rowNumber")

_rows :: forall t a r. Newtype t { rows :: a | r } => Lens' t a
_rows = _Newtype <<< prop (Proxy :: Proxy "rows")

_config :: forall t a r. Newtype t { config :: a | r } => Lens' t a
_config = _Newtype <<< prop (Proxy :: Proxy "config")

_panelsUpdated :: forall t a r. Newtype t { panelsUpdated :: a | r } => Lens' t a
_panelsUpdated = _Newtype <<< prop (Proxy :: Proxy "panelsUpdated")

_button :: forall t a r. Newtype t { button :: a | r } => Lens' t a
_button = _Newtype <<< prop (Proxy :: Proxy "button")

_name :: forall t a r. Newtype t { name :: a | r } => Lens' t a
_name = _Newtype <<< prop (Proxy :: Proxy "name")

_apiConfig :: forall t a r. Newtype t { apiConfig :: a | r } => Lens' t a
_apiConfig = _Newtype <<< prop (Proxy :: Proxy "apiConfig")

_parent :: forall t a r. Newtype t { parent :: a | r } => Lens' t a
_parent = _Newtype <<< prop (Proxy :: Proxy "parent")

_updated :: forall t a r. Newtype t { updated :: a | r } => Lens' t a
_updated = _Newtype <<< prop (Proxy :: Proxy "updated")

_deleted :: forall t a r. Newtype t { deleted :: a | r } => Lens' t a
_deleted = _Newtype <<< prop (Proxy :: Proxy "deleted")

_isActive :: forall t a r. Newtype t { isActive :: a | r } => Lens' t a
_isActive = _Newtype <<< prop (Proxy :: Proxy "isActive")

_active :: forall t a r. Newtype t { active :: a | r } => Lens' t a
_active = _Newtype <<< prop (Proxy :: Proxy "active")

_enabled :: forall t a r. Newtype t { enabled :: a | r } => Lens' t a
_enabled = _Newtype <<< prop (Proxy :: Proxy "enabled")

_floor :: forall t a r. Newtype t { floor :: a | r } => Lens' t a
_floor = _Newtype <<< prop (Proxy :: Proxy "floor")

_buttons :: forall t a r. Newtype t { buttons :: a | r } => Lens' t a
_buttons = _Newtype <<< prop (Proxy :: Proxy "buttons")

_slopeSelected :: forall t a r. Newtype t { slopeSelected :: a | r } => Lens' t a
_slopeSelected = _Newtype <<< prop (Proxy :: Proxy "slopeSelected")

_shadeSelected :: forall t a r. Newtype t { shadeSelected :: a | r } => Lens' t a
_shadeSelected = _Newtype <<< prop (Proxy :: Proxy "shadeSelected")

_shade :: forall t a r. Newtype t { shade :: a | r } => Lens' t a
_shade = _Newtype <<< prop (Proxy :: Proxy "shade")

_rating :: forall t a r. Newtype t { rating :: a | r } => Lens' t a
_rating = _Newtype <<< prop (Proxy :: Proxy "rating")

_tree :: forall t a r. Newtype t { tree :: a | r } => Lens' t a
_tree = _Newtype <<< prop (Proxy :: Proxy "tree")

_color :: forall t a r. Newtype t { color :: a | r } => Lens' t a
_color = _Newtype <<< prop (Proxy :: Proxy "color")

_vertices :: forall t a r. Newtype t { vertices :: a | r } => Lens' t a
_vertices = _Newtype <<< prop (Proxy :: Proxy "vertices")

_edge :: forall t a r. Newtype t { edge :: a | r } => Lens' t a
_edge = _Newtype <<< prop (Proxy :: Proxy "edge")

_edges :: forall t a r. Newtype t { edges :: a | r } => Lens' t a
_edges = _Newtype <<< prop (Proxy :: Proxy "edges")

_leftEdge :: forall t a r. Newtype t { leftEdge :: a | r } => Lens' t a
_leftEdge = _Newtype <<< prop (Proxy :: Proxy "leftEdge")

_rightEdge :: forall t a r. Newtype t { rightEdge :: a | r } => Lens' t a
_rightEdge = _Newtype <<< prop (Proxy :: Proxy "rightEdge")

_type :: forall t a r. Newtype t { type :: a | r } => Lens' t a
_type = _Newtype <<< prop (Proxy :: Proxy "type")

_toTarget :: forall t a r. Newtype t { toTarget :: a | r } => Lens' t a
_toTarget = _Newtype <<< prop (Proxy :: Proxy "toTarget")

_fromTarget :: forall t a r. Newtype t { fromTarget :: a | r } => Lens' t a
_fromTarget = _Newtype <<< prop (Proxy :: Proxy "fromTarget")

_buildTree :: forall t a r. Newtype t { buildTree :: a | r } => Lens' t a
_buildTree = _Newtype <<< prop (Proxy :: Proxy "buildTree")

_buildChimney :: forall t a r. Newtype t { buildChimney :: a | r } => Lens' t a
_buildChimney = _Newtype <<< prop (Proxy :: Proxy "buildChimney")

_chimney :: forall t a r. Newtype t { chimney :: a | r } => Lens' t a
_chimney = _Newtype <<< prop (Proxy :: Proxy "chimney")
