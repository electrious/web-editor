module Editor.Rendering.PanelRendering (PanelRendererConfig(..), _operations,
    _opacity, PanelRenderer, _allPanels, createPanelRenderer) where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Data.Compactable (compact)
import Data.Foldable (foldl, length, traverse_)
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
import Editor.Common.Lenses (_dragged, _parent, _tapped)
import Editor.PanelNode (PanelNode, PanelOpacity, _panel, isOpaque, mkPanelNode, updateOpacity)
import Editor.PanelOperation (ArrayOperation(..), PanelOperation(..))
import Editor.UI.DragInfo (DragInfo)
import Effect (Effect)
import FRP.Dynamic (Dynamic, dynEvent, sampleDyn, step)
import FRP.Event (Event, keepLatest)
import FRP.Event.Extra (anyEvt, foldEffect, multicast)
import Model.Hardware.PanelTextureInfo (PanelTextureInfo)
import Model.Hardware.PanelType (PanelType)
import Model.Roof.ArrayConfig (ArrayConfig)
import Model.Roof.Panel (Panel, _arrNumber, _uuid)
import Three.Core.Object3D (Object3D, add, remove)

-- | parameters for setting up a panel renderer.
newtype PanelRendererConfig = PanelRendererConfig {
    operations :: Event ArrayOperation,
    opacity    :: Dynamic PanelOpacity
}

derive instance newtypePanelRendererConfig :: Newtype PanelRendererConfig _

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

_allPanels :: forall t a r. Newtype t { allPanels :: a | r } => Lens' t a
_allPanels = _Newtype <<< prop (SProxy :: SProxy "allPanels")

-- | internal state of the panel renderer, which manages all rendered panels
-- and temporary panels
newtype RendererState = RendererState {
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
data RendererOp = ArrayOp ArrayOperation ArrayConfig PanelType PanelOpacity
                | UpdateArrayConfig ArrayConfig PanelType PanelOpacity
                | UpdateOpacity PanelOpacity

updateStateWithOp :: PanelTextureInfo -> RendererOp -> RendererState -> Effect RendererState
updateStateWithOp textInfo (ArrayOp op arrCfg panelType opacity) st = updateStateWithArrayOp textInfo arrCfg panelType opacity op st
updateStateWithOp textInfo (UpdateArrayConfig arrCfg panelType opacity) st = do
    let ps = values $ view _panel <$> st ^. _renderedPanels
    traverse_ (flip remove (st ^. _parent)) (st ^. _renderedPanels)
    nps <- traverse (renderPanelNode arrCfg textInfo panelType opacity st) ps
    pure $ st # _renderedPanels .~ panelNodeList2Map (compact nps)
updateStateWithOp _ (UpdateOpacity op) st = traverse (updateOpacity op) (st ^. _renderedPanels) *> pure st


renderPanelNode :: ArrayConfig -> PanelTextureInfo -> PanelType -> PanelOpacity -> RendererState -> Panel -> Effect (Maybe PanelNode)
renderPanelNode arrCfg textInfo panelType opacity st p = 
    if (not $ panelRendered p st)
        then do
            pn <- mkPanelNode arrCfg textInfo panelType p false
            updateOpacity opacity pn
            enableShadows (isOpaque opacity) pn
            add pn (st ^. _parent)
            pure $ Just pn
        else pure Nothing

-- | process array operations and update the internal renderer state.
updateStateWithPanelOp :: PanelTextureInfo -> ArrayConfig -> PanelType -> PanelOpacity -> PanelOperation -> RendererState -> Effect RendererState
updateStateWithPanelOp textInfo arrCfg panelType opacity (LoadPanels ps) st = do
    pns <- traverse (renderPanelNode arrCfg textInfo panelType opacity st) ps
    pure $ st # _renderedPanels %~ union (panelNodeList2Map $ compact pns)
updateStateWithPanelOp textInfo arrCfg panelType opacity (AddPanel p) st = do
    pn <- renderPanelNode arrCfg textInfo panelType opacity st p
    case pn of
        Just n -> pure $ st # _renderedPanels %~ insert (p ^. _uuid) n
        Nothing -> pure st
updateStateWithPanelOp textInfo arrCfg panelType opacity (AddPanels ps) st = do
    pns <- traverse (renderPanelNode arrCfg textInfo panelType opacity st) ps
    pure $ st # _renderedPanels %~ union (panelNodeList2Map $ compact pns)
updateStateWithPanelOp _ _ _ _ (DelPanel pid) st =
    case lookup pid (st ^. _renderedPanels) of
        Nothing -> pure st
        Just pn -> do remove pn (st ^. _parent)
                      pure $ st # _renderedPanels %~ delete pid
updateStateWithPanelOp _ _ _ _ (DelPanels pids) st = do
    let doRemove pid = case lookup pid (st ^. _renderedPanels) of
                        Nothing -> pure Nothing
                        Just pn -> remove pn (st ^. _parent) *> pure (Just pid)
    npids <- compact <$> traverse doRemove pids
    pure $ st # _renderedPanels %~ flip (foldl (flip delete)) npids
updateStateWithPanelOp _ _ _ _ DeleteAll st = do traverse_ (flip remove (st ^. _parent)) (st ^. _renderedPanels)
                                                 pure $ st # _renderedPanels .~ Map.empty
updateStateWithPanelOp _ arrCfg _ _ (UpdatePanels ps) st = do
    let updM m pn = update (const $ Just pn) (pn ^. _panel <<< _uuid) m
        pns = compact $ (\p -> Tuple p <$> lookup (p ^. _uuid) (st ^. _renderedPanels)) <$> ps
    newPs <- traverse (\(Tuple p pn) -> changePanel arrCfg p pn) pns
    pure $ st # _renderedPanels %~ flip (foldl updM) newPs

updateStateWithArrayOp :: PanelTextureInfo -> ArrayConfig -> PanelType -> PanelOpacity -> ArrayOperation -> RendererState -> Effect RendererState
updateStateWithArrayOp textInfo arrCfg panelType opacity (PanelOperation op) st = updateStateWithPanelOp textInfo arrCfg panelType opacity op st
updateStateWithArrayOp textInfo arrCfg panelType _ (TempPanels ps) st = renderTempPanels textInfo arrCfg panelType ps st
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
              # _tempPanelNodes .~ Nil


-- | render temporary panels
renderTempPanels :: PanelTextureInfo -> ArrayConfig -> PanelType -> List Panel -> RendererState -> Effect RendererState
renderTempPanels textInfo arrCfg panelType ps st = do
    let oldNum = length $ st ^. _tempPanelNodes
        newNum = length ps

        updNode (Tuple node p) = changePanel arrCfg p node
        mkNode p = do
            n <- mkPanelNode arrCfg textInfo panelType p true
            add n (st ^. _parent)
            pure n
        delNode n = remove n (st ^. _parent)
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
        stateEvt = multicast $ foldEffect (updateStateWithOp textureInfo) (arrayOpEvt <|> arrCfgOpEvt <|> opacityOpEvt) defState
        statesDyn = step defState stateEvt

        panelNodesEvt = multicast $ values <<< view _renderedPanels <$> stateEvt

    pure $ PanelRenderer {
        tapped    : multicast $ keepLatest $ anyEvt <<< map (view _tapped) <$> panelNodesEvt,
        dragged   : multicast $ keepLatest $ anyEvt <<< map (view _dragged) <$> panelNodesEvt,
        allPanels : allPanelsRendered <$> statesDyn
    }
