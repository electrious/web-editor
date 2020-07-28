module Editor.Rendering.PanelRendering where

import Data.Lens (Lens', Lens)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID)
import Editor.ArrayBuilder (ArrayBuilder(..))
import Editor.PanelNode (PanelNode(..), mkPanelMesh)
import Editor.UI.DragInfo (DragInfo)
import Effect (Effect)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Model.Hardware.PanelType (PanelType)
import Model.Roof.ArrayConfig (ArrayConfig)
import Model.Roof.Panel (Panel)
import Three.Core.Object3D (Object3D)
import Three.Math.Vector (Vector3)

data PanelOperation = AddPanel Panel
                    | AddTempPanel Panel
                    | DelPanel UUID
                    | DeleteAll
                    | UpdatePanel Panel
                    | MoveArray Int Vector3
                    | PreserveTempPanels

data PanelOpacity = Opaque
                  | Transparent


newtype PanelRendererConfig = PanelRendererConfig {
    parent      :: Object3D,
    operations  :: Event PanelOperation,
    opacity     :: Dynamic PanelOpacity
}

derive instance newtypePanelRendererConfig :: Newtype PanelRendererConfig _

_parent :: forall t a r. Newtype t { parent :: a | r } => Lens' t a
_parent = _Newtype <<< prop (SProxy :: SProxy "parent")

_operations :: forall t a r. Newtype t { operations :: a | r } => Lens' t a
_operations = _Newtype <<< prop (SProxy :: SProxy "operations")

_opacity :: forall t a r. Newtype t { opacity :: a | r } => Lens' t a
_opacity = _Newtype <<< prop (SProxy :: SProxy "opacity")

newtype PanelRenderer = PanelRenderer {
    panelTapped  :: Event Panel,
    panelDragged :: Event (DragInfo Panel),
    allPanels    :: Dynamic (List Panel)
}

derive instance newtypePanelRenderer :: Newtype PanelRenderer _

_panelTapped :: forall t a r. Newtype t { panelTapped :: a | r } => Lens' t a
_panelTapped = _Newtype <<< prop (SProxy :: SProxy "panelTapped")

_panelDragged :: forall t a r. Newtype t { panelDragged :: a | r } => Lens' t a
_panelDragged = _Newtype <<< prop (SProxy :: SProxy "panelDragged")

_allPanels :: forall t a r. Newtype t { allPanels :: a | r } => Lens' t a
_allPanels = _Newtype <<< prop (SProxy :: SProxy "allPanels")

newtype RendererState = RendererState {
    renderedPanels :: Map UUID PanelNode,
    tempPanelNodes :: List PanelNode
}

derive instance newtypeRendererState :: Newtype RendererState _

_renderedPanels :: forall t a r. Newtype t { renderedPanels :: a | r } => Lens' t a
_renderedPanels = _Newtype <<< prop (SProxy :: SProxy "renderedPanels")

_tempPanelNodes :: forall t a r. Newtype t { tempPanelNodes :: a | r } => Lens' t a
_tempPanelNodes = _Newtype <<< prop (SProxy :: SProxy "tempPanelNodes")

updateStateWithOp :: RendererState -> PanelOperation -> Effect RendererState
updateStateWithOp st (AddPanel p) = do
    pnDyn <- mkPanelMesh p
    

createPanelRenderer :: PanelRendererConfig -> ArrayBuilder PanelRenderer
createPanelRenderer cfg = do
