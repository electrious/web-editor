module Editor.Rendering.PanelRendering where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Compactable (compact)
import Data.Foldable (class Foldable, foldl, foldlDefault, length, traverse_)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), drop, zip)
import Data.Map (Map, delete, fromFoldable, insert, lookup, member, union, update, values)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Triple (Triple(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Editor.ArrayBuilder (ArrayBuilder, getArrayConfig, getPanelType, getTextureInfo)
import Editor.Common.Lenses (_dragged, _tapped)
import Editor.PanelNode (PanelNode, PanelOpacity, _panel, _panelObject, changePanel, changeToNormal, enableShadows, isOpaque, mkPanelNode, moveBy, updateOpacity)
import Editor.UI.DragInfo (DragInfo)
import Effect (Effect)
import FRP.Dynamic (Dynamic, dynEvent, sampleDyn, step)
import FRP.Event (Event, keepLatest)
import FRP.Event.Extra (foldEffect)
import Model.Hardware.PanelTextureInfo (PanelTextureInfo)
import Model.Hardware.PanelType (PanelType)
import Model.Roof.ArrayConfig (ArrayConfig)
import Model.Roof.Panel (Panel, _arrNumber, _uuid)
import Three.Core.Object3D (Object3D, add, remove)
import Three.Math.Vector (Vector3)

-- | Panel operation that change a panel array.
data PanelOperation = AddPanel Panel
                    | AddPanels (List Panel)
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
    tapped    :: Event Panel,
    dragged   :: Event (DragInfo Panel),
    allPanels :: Dynamic (List Panel)
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

allPanelsRendered :: RendererState -> List Panel
allPanelsRendered st = view _panel <$> values (st ^. _renderedPanels)

panelNodeList2Map :: List PanelNode -> Map UUID PanelNode
panelNodeList2Map = fromFoldable <<< map mkT
    where mkT pn = Tuple (pn ^. _panel ^. _uuid) pn

-- | data type used to update the panel renderer's internal state
data RendererOp = ArrayOp PanelOperation ArrayConfig PanelType PanelOpacity
                | UpdateArrayConfig ArrayConfig PanelType PanelOpacity
                | UpdateOpacity PanelOpacity

updateStateWithOp :: PanelTextureInfo -> RendererOp -> RendererState -> Effect RendererState
updateStateWithOp textInfo (ArrayOp op arrCfg panelType opacity) st = updateStateWithArrayOp textInfo arrCfg panelType opacity op st
updateStateWithOp textInfo (UpdateArrayConfig arrCfg panelType opacity) st = do
    let ps = values $ view _panel <$> st ^. _renderedPanels
    traverse_ (remove (st ^. _parent) <<< view _panelObject) (st ^. _renderedPanels)
    nps <- traverse (renderPanelNode arrCfg textInfo panelType opacity st) ps
    pure $ st # _renderedPanels .~ panelNodeList2Map (compact nps)
updateStateWithOp _ (UpdateOpacity op) st = traverse (updateOpacity op) (st ^. _renderedPanels) *> pure st


renderPanelNode :: ArrayConfig -> PanelTextureInfo -> PanelType -> PanelOpacity -> RendererState -> Panel -> Effect (Maybe PanelNode)
renderPanelNode arrCfg textInfo panelType opacity st p = 
    if (not $ panelRendered p st)
        then do
            pn <- mkPanelNode arrCfg textInfo panelType p
            updateOpacity opacity pn
            enableShadows (isOpaque opacity) pn
            add (st ^. _parent) (pn ^. _panelObject)
            pure $ Just pn
        else pure Nothing

-- | process array operations and update the internal renderer state.
updateStateWithArrayOp :: PanelTextureInfo -> ArrayConfig -> PanelType -> PanelOpacity -> PanelOperation -> RendererState -> Effect RendererState
updateStateWithArrayOp textInfo arrCfg panelType opacity (AddPanel p) st = do
    pn <- renderPanelNode arrCfg textInfo panelType opacity st p
    case pn of
        Just n -> pure $ st # _renderedPanels %~ insert (p ^. _uuid) n
        Nothing -> pure st
updateStateWithArrayOp textInfo arrCfg panelType opacity (AddPanels ps) st = do
    pns <- traverse (renderPanelNode arrCfg textInfo panelType opacity st) ps
    pure $ st # _renderedPanels %~ union (panelNodeList2Map $ compact pns)
updateStateWithArrayOp textInfo arrCfg panelType _ (TempPanels ps) st = renderTempPanels textInfo arrCfg panelType ps st
updateStateWithArrayOp _ _ _ _ (DelPanel pid) st =
    case lookup pid (st ^. _renderedPanels) of
        Nothing -> pure st
        Just pn -> do remove (st ^. _parent) (pn ^. _panelObject)
                      pure $ st # _renderedPanels %~ delete pid
updateStateWithArrayOp _ _ _ _ DeleteAll st = do traverse_ (remove (st ^. _parent) <<< view _panelObject) (st ^. _renderedPanels)
                                                 pure $ st # _renderedPanels .~ Map.empty
updateStateWithArrayOp _ arrCfg _ _ (UpdatePanel p) st =
    case lookup (p ^. _uuid) (st ^. _renderedPanels) of
        Nothing -> pure st
        Just pn -> do newPn <- changePanel arrCfg p pn
                      pure $ st # _renderedPanels %~ update (const $ Just newPn) (p ^. _uuid)
updateStateWithArrayOp _ _ _ _ (MoveArray arr delta) st = do
    let ps = st ^. _renderedPanels
        doMove pn = if pn ^. (_panel <<< _arrNumber) == arr
                    then moveBy delta pn
                    else pure pn
    newPs <- traverse doMove ps
    pure $ st # _renderedPanels .~ newPs
updateStateWithArrayOp _ _ _ _ PreserveTempPanels st = do
    let ps = st ^. _tempPanelNodes
        f m n = insert (n ^. (_panel <<< _uuid)) n m
    newPs <- traverse changeToNormal ps
    pure $ st # _renderedPanels %~ flip (foldl f) newPs


-- | render temporary panels
renderTempPanels :: PanelTextureInfo -> ArrayConfig -> PanelType -> List Panel -> RendererState -> Effect RendererState
renderTempPanels textInfo arrCfg panelType ps st = do
    let oldNum = length $ st ^. _tempPanelNodes
        newNum = length ps

        updNode (Tuple node p) = changePanel arrCfg p node
        mkNode p = do
            n <- mkPanelNode arrCfg textInfo panelType p
            add (st ^. _parent) (n ^. _panelObject)
            pure n
        delNode n = remove (st ^. _parent) (n ^. _panelObject)
    if newNum > oldNum
      then do
        updNodes <- traverse updNode $ zip (st ^. _tempPanelNodes) ps
        newNodes <- traverse mkNode $ drop oldNum ps
        pure $ st # _tempPanelNodes .~ append updNodes newNodes
      else do
        updNodes <- traverse updNode $ zip (st ^. _tempPanelNodes) ps
        traverse_ delNode $ drop newNum (st ^. _tempPanelNodes)
        pure $ st # _tempPanelNodes .~ updNodes


createPanelRenderer :: PanelRendererConfig -> ArrayBuilder PanelRenderer
createPanelRenderer cfg = do
    arrCfgDyn    <- getArrayConfig
    textureInfo  <- getTextureInfo
    panelTypeDyn <- getPanelType
    
    let opacityDyn = cfg ^. _opacity

        mkArrayOp op (Triple arrCfg pt opacity) = ArrayOp op arrCfg pt opacity
        arrayOpEvt = sampleDyn (Triple <$> arrCfgDyn <*> panelTypeDyn <*> opacityDyn) (mkArrayOp <$> cfg ^. _operations)

        mkArrCfgOp arrCfg (Tuple pt opacity) = UpdateArrayConfig arrCfg pt opacity
        arrCfgOpEvt = sampleDyn (Tuple <$> panelTypeDyn <*> opacityDyn) (mkArrCfgOp <$> dynEvent arrCfgDyn)

        opacityOpEvt = UpdateOpacity <$> dynEvent opacityDyn

        defState = RendererState {
            parent         : cfg ^. _parent,
            renderedPanels : Map.empty,
            tempPanelNodes : Nil
        }
        stateEvt = foldEffect (updateStateWithOp textureInfo) (arrayOpEvt <|> arrCfgOpEvt <|> opacityOpEvt) defState
        statesDyn = step defState stateEvt

        panelNodesEvt = (values <<< view _renderedPanels) <$> stateEvt


    pure $ PanelRenderer {
        tapped    : keepLatest $ leftmost <<< map (view _tapped) <$> panelNodesEvt,
        dragged   : keepLatest $ leftmost <<< map (view _dragged) <$> panelNodesEvt,
        allPanels : allPanelsRendered <$> statesDyn
    }


leftmost :: forall a f. Foldable f => f (Event a) -> Event a
leftmost = foldlDefault (<|>) empty
