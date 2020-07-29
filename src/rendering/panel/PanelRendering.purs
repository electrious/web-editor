module Editor.Rendering.PanelRendering where

import Prelude

import Data.Default (class Default, def)
import Data.Foldable (traverse_)
import Data.Lens (Lens', (^.), (%~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Map (Map, delete, insert, lookup, member)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID)
import Editor.ArrayBuilder (ArrayBuilder(..), getArrayConfig, getPanelType, getTextureInfo)
import Editor.PanelNode (PanelNode(..), PanelOpacity, _panelObject, enableShadows, isOpaque, mkPanelMesh, updateOpacity)
import Editor.UI.DragInfo (DragInfo)
import Effect (Effect)
import FRP.Dynamic (Dynamic, fold, step)
import FRP.Event (Event)
import FRP.Event.Extra (foldEffect)
import Model.Hardware.PanelTextureInfo (PanelTextureInfo(..))
import Model.Hardware.PanelType (PanelType)
import Model.Roof.ArrayConfig (ArrayConfig)
import Model.Roof.Panel (Panel, _uuid)
import Three.Core.Material (setOpacity)
import Three.Core.Object3D (Object3D, add, parent, remove)
import Three.Math.Vector (Vector3)

-- | Panel operation that change a panel array.
data PanelOperation = AddPanel Panel
                    | TempPanels (List Panel)
                    | DelPanel UUID
                    | DeleteAll
                    | UpdatePanel Panel
                    | MoveArray Int Vector3
                    | PreserveTempPanels

-- | parameters for setting up a panel renderer.
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

-- | Panel Renderer that manages all panels rendered
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

-- | internal state of the panel renderer, which manages all rendered panels
-- and temporary panels
newtype RendererState = RendererState {
    parent         :: Object3D,
    renderedPanels :: Map UUID PanelNode,
    tempPanelNodes :: List PanelNode
}

derive instance newtypeRendererState :: Newtype RendererState _

_renderedPanels :: forall t a r. Newtype t { renderedPanels :: a | r } => Lens' t a
_renderedPanels = _Newtype <<< prop (SProxy :: SProxy "renderedPanels")

_tempPanelNodes :: forall t a r. Newtype t { tempPanelNodes :: a | r } => Lens' t a
_tempPanelNodes = _Newtype <<< prop (SProxy :: SProxy "tempPanelNodes")

panelRendered :: Panel -> RendererState -> Boolean
panelRendered p st = member (p ^. _uuid) (st ^. _renderedPanels)

-- | data type used to update the panel renderer's internal state
data RendererOp = ArrayOp PanelOperation ArrayConfig PanelType PanelOpacity
                | UpdateArrayConfig ArrayConfig
                | UpdateOpacity PanelOpacity

updateStateWithOp :: PanelTextureInfo -> RendererOp -> RendererState -> Effect RendererState
updateStateWithOp textInfo (ArrayOp op arrCfg panelType) st = updateStateWithArrayOp textInfo arrCfg panelType op st


updateStateWithArrayOp :: PanelTextureInfo -> ArrayConfig -> PanelType -> PanelOpacity -> PanelOperation -> RendererState -> Effect RendererState
updateStateWithArrayOp textInfo arrCfg panelType opacity (AddPanel p) st =
    if (not $ panelRendered p st)
        then do
            pn <- mkPanelMesh arrCfg textInfo panelType p
            updateOpacity opacity pn
            enableShadows (isOpaque opacity) pn
            add (st ^. _parent) (pn ^. _panelObject)
            pure $ st # _renderedPanels %~ insert (p ^. _uuid) pn
        else pure st
updateStateWithArrayOp textInfo arrCfg panelType _ (TempPanels ps) st = renderTempPanels textInfo arrCfg panelType ps st
updateStateWithArrayOp _ _ _ _ (DelPanel pid) st =
    case lookup pid (st ^. _renderedPanels) of
        Nothing -> pure st
        Just pn -> do remove (st ^. _parent) (pn ^. _panelObject)
                      pure $ st # _renderedPanels %~ delete pid
updateStateWithArrayOp _ _ _ _ DeleteAll st = do traverse_ (remove (st ^. _parent)) (st ^. _renderedPanels)
                                                 pure $ st # _renderedPanels .~ Map.empty

createPanelRenderer :: PanelRendererConfig -> ArrayBuilder PanelRenderer
createPanelRenderer cfg = do
    arrCfgDyn    <- getArrayConfig
    textureInfo  <- getTextureInfo
    panelTypeDyn <- getPanelType
    
    let statesEvt = foldEffect (updateStateWithOp textureInfo) (cfg ^. _operations) def

    pure $ PanelRenderer {

    }
