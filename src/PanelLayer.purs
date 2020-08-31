module Editor.PanelLayer where

import Prelude hiding (add)

import API (APIConfig)
import Algorithm.ButtonCalculator (plusBtnsForArray, rotateBtnsForArray)
import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (class Foldable, foldl, null)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Map (size)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID)
import Editor.ArrayBuilder (ArrayBuilder, _arrayConfig, liftRenderingM)
import Editor.Common.Lenses (_apiConfig, _disposable, _object, _panels, _panelsUpdated, _rackingType, _roof)
import Editor.Disposable (class Disposable)
import Editor.PanelAPIInterpreter (_finished, mkPanelAPIInterpreter)
import Editor.PanelArrayLayout (PanelsLayout, _arrays, _tree, defaultLayout, findActiveArray, getArrayAt, layoutPanels)
import Editor.PanelNode (PanelOpacity)
import Editor.PanelOperation (ArrayOperation, PanelOperation(..))
import Editor.Rendering.ButtonsRenderer (ButtonOperation(..), mkButtonsRenderer)
import Editor.Rendering.PanelRendering (PanelRenderer, PanelRendererConfig(..), _opacity, _operations, createPanelRenderer)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, step)
import FRP.Event (Event)
import Model.Hardware.PanelModel (PanelModel)
import Model.PanelArray (PanelArray)
import Model.Racking.RackingType (RackingType(..))
import Model.Roof.ArrayConfig (ArrayConfig)
import Model.Roof.Panel (Orientation(..), Panel, _uuid)
import Model.Roof.RoofPlate (RoofPlate)
import Model.UpdatedPanels (delete, deletePanels, get, toUnfoldable)
import Three.Core.Object3D (class IsObject3D, Object3D, mkObject3D, setName)

newtype PanelLayerConfig = PanelLayerConfig {
    roof            :: RoofPlate,
    roofActive      :: Dynamic Boolean,
    mainOrientation :: Dynamic Orientation,
    arrayConfig     :: Dynamic ArrayConfig,
    panelType       :: Dynamic PanelModel,
    initPanels      :: Event (List Panel),
    opacity         :: Dynamic PanelOpacity,
    apiConfig       :: APIConfig
}

derive instance newtypePanelLayerConfig :: Newtype PanelLayerConfig _

_roofActive :: forall t a r. Newtype t { roofActive :: a | r } => Lens' t a
_roofActive = _Newtype <<< prop (SProxy :: SProxy "roofActive")

_mainOrientation :: forall t a r. Newtype t { mainOrientation :: a | r } => Lens' t a
_mainOrientation = _Newtype <<< prop (SProxy :: SProxy "mainOrientation")

_initPanels :: forall t a r. Newtype t { initPanels :: a | r } => Lens' t a
_initPanels = _Newtype <<< prop (SProxy :: SProxy "initPanels")

newtype PanelLayer = PanelLayer {
    object              :: Object3D,
    disposable          :: Effect Unit,

    roof                :: RoofPlate, 
    arrayConfig         :: Dynamic ArrayConfig,

    arrayChanged        :: Event Unit,
    serverUpdated       :: Event Unit,
    arrayDragging       :: Dynamic Boolean,
    inactiveArrayTapped :: Event Unit
}

derive instance newtypePanelLayer :: Newtype PanelLayer _
instance isObject3DPanelLayer :: IsObject3D PanelLayer where
    toObject3D = view _object
instance disposablePanelLayer :: Disposable PanelLayer where
    dispose = view _disposable

_layout :: forall t a r. Newtype t { layout :: a | r } => Lens' t a
_layout = _Newtype <<< prop (SProxy :: SProxy "layout")

_arrayChanged :: forall t a r. Newtype t { arrayChanged :: a | r } => Lens' t a
_arrayChanged = _Newtype <<< prop (SProxy :: SProxy "arrayChanged")

_serverUpdated :: forall t a r. Newtype t {serverUpdated :: a | r } => Lens' t a
_serverUpdated = _Newtype <<< prop (SProxy :: SProxy "serverUpdated")

_arrayDragging :: forall t a r. Newtype t { arrayDragging :: a | r } => Lens' t a
_arrayDragging = _Newtype <<< prop (SProxy :: SProxy "arrayDragging")

_inactiveArrayTapped :: forall t a r. Newtype t { inactiveArrayTapped :: a | r } => Lens' t a
_inactiveArrayTapped = _Newtype <<< prop (SProxy :: SProxy "inactiveArrayTapped")


-- | internal panel layer state data structure
newtype PanelLayerState = PanelLayerState {
    arrayConfig      :: ArrayConfig,

    roofActive       :: Boolean,
    activeArray      :: Maybe Int,

    layout           :: PanelsLayout,
    orientationToUse :: Maybe Orientation,

    panelOperations  :: List PanelOperation,
    arrayOperations  :: List ArrayOperation,
    btnsOperations   :: List ButtonOperation
}

derive instance newtypePanelLayerState :: Newtype PanelLayerState _
instance defaultPanelLayerState :: Default PanelLayerState where
    def = PanelLayerState {
        arrayConfig      : def,
        roofActive       : false,
        activeArray      : Nothing,
        layout           : def,
        orientationToUse : Nothing,
        panelOperations  : Nil,
        arrayOperations  : Nil,
        btnsOperations   : Nil
    }

_activeArray :: forall t a r. Newtype t { activeArray :: a | r } => Lens' t a
_activeArray = _Newtype <<< prop (SProxy :: SProxy "activeArray")

_orientationToUse :: forall t a r. Newtype t { orientationToUse :: a | r } => Lens' t a
_orientationToUse = _Newtype <<< prop (SProxy :: SProxy "orientationToUse")

_panelOperations :: forall t a r. Newtype t { panelOperations :: a | r } => Lens' t a
_panelOperations = _Newtype <<< prop (SProxy :: SProxy "panelOperations")

_arrayOperations :: forall t a r. Newtype t { arrayOperations :: a | r } => Lens' t a
_arrayOperations = _Newtype <<< prop (SProxy :: SProxy "arrayOperations")

_btnsOperations :: forall t a r. Newtype t { btnsOperations :: a | r } => Lens' t a
_btnsOperations = _Newtype <<< prop (SProxy :: SProxy "btnsOperations")

createPanelLayer :: PanelLayerConfig -> ArrayBuilder PanelLayer
createPanelLayer cfg = do
    layer <- liftEffect mkObject3D
    liftEffect $ setName "panel-layer" layer

    let roof       = cfg ^. _roof
        arrCfgDyn  = cfg ^. _arrayConfig
        arrOpEvt   = empty
        panelOpEvt = empty
        btnOpEvt   = empty

    panelRenderer <- setupPanelRenderer layer arrOpEvt (cfg ^. _opacity)
    btnsRenderer  <- liftRenderingM $ mkButtonsRenderer layer btnOpEvt
    let apiInterpreter = mkPanelAPIInterpreter $ def # _apiConfig  .~ cfg ^. _apiConfig
                                                     # _roof       .~ roof
                                                     # _panels     .~ Nil
                                                     # _operations .~ panelOpEvt

        layoutDyn = step Nothing empty

    pure $ PanelLayer {
        object              : layer,
        disposable          : pure unit,

        roof                : roof,
        arrayConfig         : arrCfgDyn,
        arrayChanged        : empty,
        serverUpdated       : apiInterpreter ^. _finished,
        arrayDragging       : step false empty,
        inactiveArrayTapped : empty
    }


setupPanelRenderer :: Object3D -> Event ArrayOperation -> Dynamic PanelOpacity -> ArrayBuilder PanelRenderer
setupPanelRenderer parent opEvt opacity = createPanelRenderer $ PanelRendererConfig {
        parent     : parent,
        operations : opEvt,
        opacity    : opacity
    }


-- | function to update the ArrayLayout data structure 
updateLayout :: forall f. Foldable f => Functor f => PanelLayerState -> f Panel -> Effect PanelsLayout
updateLayout st ps = if null ps
                     then defaultLayout orient cfg
                     else layoutPanels ps cfg
    where cfg = st ^. _arrayConfig
          orient = if cfg ^. _rackingType == BX  -- always use landscape for BX system
                   then Landscape
                   else fromMaybe Landscape $ st ^. _orientationToUse

-- | get all panels in the current state
allPanels :: PanelLayerState -> List Panel
allPanels st = st ^. (_layout <<< _panels)

-- | add a new panel to the current state
addPanel :: PanelLayerConfig -> PanelLayerState -> Panel -> Effect PanelLayerState
addPanel cfg st p = do
    let oldPs = allPanels st
        ps    = p : oldPs

    -- update layout with new panels
    newLayout <- updateLayout st ps

    -- get the updated new panel
    let updatedPs = newLayout ^. _panelsUpdated
        newP      = fromMaybe p $ get (p ^. _uuid) updatedPs
        newUpdPs  = delete (p ^. _uuid) updatedPs

        addNewPOp = AddPanel newP
        updOps    = UpdatePanels $ toUnfoldable newUpdPs

    checkAndUpdateBtnOps cfg true $ st # _panelOperations .~ addNewPOp : updOps : Nil
                                       # _layout          .~ newLayout


addPanels :: PanelLayerConfig -> PanelLayerState -> List Panel -> Effect PanelLayerState
addPanels cfg st ps = do
    let oldPs = allPanels st
        newPs = append ps oldPs

    -- update layout with new panels
    newLayout <- updateLayout st newPs

    let updPs = newLayout ^. _panelsUpdated
        f ops p = case get (p ^. _uuid) updPs of
                    Just np -> np : ops
                    Nothing -> ops
        nps = foldl f Nil ps
        updatedPs = deletePanels (view _uuid <$> nps) updPs

        newPsOp = AddPanels nps
        updOps  = UpdatePanels $ toUnfoldable updatedPs
    
    checkAndUpdateBtnOps cfg true $ st # _panelOperations .~ newPsOp : updOps : Nil
                                       # _layout          .~ newLayout

deletePanel :: PanelLayerConfig -> PanelLayerState -> UUID -> Effect PanelLayerState
deletePanel cfg st pid = do
    let ps = filter ((/=) pid <<< view _uuid) $ allPanels st
    
    newLayout <- updateLayout st ps
    let delPOp = DelPanel pid
        updOps = UpdatePanels $ toUnfoldable (newLayout ^. _panelsUpdated)
    
    checkAndUpdateBtnOps cfg true $ st # _panelOperations .~ delPOp : updOps : Nil
                                       # _layout          .~ newLayout


checkAndUpdateBtnOps :: PanelLayerConfig -> Boolean -> PanelLayerState -> Effect PanelLayerState
checkAndUpdateBtnOps cfg arrayChanged st = if st ^. _roofActive
    then do
        let layout = st ^. _layout
            actArr = fromMaybe 1000000 $ st ^. _activeArray
            arrCnt = size $ layout ^. _arrays
            newActArr = if arrayChanged && actArr >= arrCnt then findActiveArray layout else actArr
            arr = getArrayAt newActArr layout
        case arr of
            Just a -> do btnOps <- btnOpsForArray cfg st a
                         pure $ st # _btnsOperations .~ btnOps
                                   # _activeArray    .~ Just newActArr
            Nothing -> pure st
    else pure st

-- | calculate button operations for a panel array
btnOpsForArray :: PanelLayerConfig -> PanelLayerState -> PanelArray -> Effect (List ButtonOperation)
btnOpsForArray cfg st arr = do
    plusBtns <- plusBtnsForArray arr (st ^. _layout <<< _tree) (cfg ^. _roof)
    rotBtns  <- rotateBtnsForArray arr plusBtns
    let reset  = ResetButtons
        plusOp = RenderPlusButtons plusBtns
        rotOp  = RenderRotateButtons rotBtns
    pure $ reset : plusOp : rotOp : Nil
