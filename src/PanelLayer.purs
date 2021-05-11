module Editor.PanelLayer where

import Prelude hiding (add)

import Algorithm.ButtonCalculator (plusBtnsForArray, rotateBtnsForArray)
import Algorithm.PanelAligning (alignPanelRows)
import Algorithm.TempPanels (tempPanels)
import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Custom.Mesh (DraggableMesh, mkDraggableMesh)
import Data.Default (def)
import Data.Filterable (compact, filter)
import Data.Foldable (class Foldable, any, null)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', set, view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), fromFoldable, partition, singleton, (:))
import Data.List.Partial (head)
import Data.Map (lookup, size)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Meter (meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, genUUID)
import Editor.ArrayBuilder (ArrayBuilder, _arrayConfig, _editorMode, _rotateButtonMaterial, liftRenderingM)
import Editor.Common.Lenses (_alignment, _apiConfig, _arrayNumber, _disposable, _dragType, _dragged, _houseId, _id, _object, _orientation, _panels, _panelsUpdated, _point, _rackingType, _roof, _rowNumber, _rows, _slope, _tapped, _x, _y)
import Editor.Disposable (class Disposable)
import Editor.EditorMode (EditorMode(..))
import Editor.Input.Commoon (DragType(..))
import Editor.PanelAPIInterpreter (PanelAPIInterpreter, _finished, mkPanelAPIInterpreter)
import Editor.PanelArrayLayout (PanelsLayout, _arrays, _tree, defaultLayout, findActiveArray, getArrayAt, layoutPanels, neighbors)
import Editor.PanelNode (PanelOpacity)
import Editor.PanelOperation (ArrayOperation(..), PanelOperation(..))
import Editor.Rendering.ButtonsRenderer (ButtonOperation(..), ButtonsRenderer, _plusDragged, _plusTapped, _rotTapped, mkButtonsRenderer)
import Editor.Rendering.PanelRendering (PanelRenderer, PanelRendererConfig(..), _opacity, _operations, createPanelRenderer)
import Editor.SceneEvent (isDrag, isDragEnd, isDragStart)
import Editor.UI.DragInfo (DragInfo, mkDragInfo)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, current, dynEvent, gateDyn, sampleDyn, step, subscribeDyn)
import FRP.Event (Event, create, sampleOn, subscribe)
import FRP.Event.Extra (debounce, foldEffect, fromFoldableE, multicast, performEvent, skip)
import Model.ArrayComponent (arrayNumber)
import Model.Hardware.PanelModel (PanelModel)
import Model.PanelArray (PanelArray, rotateRow)
import Model.PlusButton (PlusButton)
import Model.Racking.RackingType (RackingType(..))
import Model.Roof.ArrayConfig (ArrayConfig)
import Model.Roof.Panel (Alignment(..), Orientation(..), Panel, _arrNumber, _roofUUID, _uuid, _roofplateId, addDelta, panelVertices)
import Model.Roof.RoofPlate (RoofPlate, _roofIntId, isFlat)
import Model.Roof.RoofPlateTransform (wrapAroundPoints)
import Model.UpdatedPanels (delete, deletePanels, get, merge, toUnfoldable)
import Model.UpdatedPanels as UpdatePanels
import Model.UpdatedPanels as UpdatedPanels
import Partial.Unsafe (unsafePartial)
import Rendering.Renderable (withRenderContext)
import Three.Core.Geometry (mkBoxGeometry)
import Three.Core.Material (mkMeshBasicMaterial, setOpacity, setTransparent)
import Three.Core.Object3D (class IsObject3D, Object3D, add, mkObject3D, setCastShadow, setName, setRenderOrder, setVisible, worldToLocal)
import Three.Math.Vector (Vector3, mkVec3, (<->))

newtype PanelLayerConfig = PanelLayerConfig {
    houseId         :: Int,
    roof            :: RoofPlate,
    roofActive      :: Dynamic Boolean,
    mainOrientation :: Dynamic Orientation, -- project wise orientation used for new array
    orientation     :: Dynamic Orientation, -- current orientation of the roof
    alignment       :: Dynamic Alignment,
    panelType       :: Dynamic PanelModel,
    initPanels      :: Event (List Panel),
    opacity         :: Dynamic PanelOpacity
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
    renderer            :: PanelRenderer,
    btnsRenderer        :: ButtonsRenderer,
    apiInterpreter      :: PanelAPIInterpreter,

    disposable          :: Effect Unit,

    roof                :: RoofPlate,

    arrayChanged        :: Event Unit,
    serverUpdated       :: Event Unit,
    arrayDragging       :: Dynamic Boolean,
    plusDragging        :: Dynamic Boolean,
    inactiveRoofTapped  :: Event Unit,
    activeArray         :: Event (Maybe PanelArray),

    currentPanels       :: Event (List Panel)
}

derive instance newtypePanelLayer :: Newtype PanelLayer _
instance isObject3DPanelLayer :: IsObject3D PanelLayer where
    toObject3D = view _object
instance disposablePanelLayer :: Disposable PanelLayer where
    dispose = view _disposable

defPanelLayerWith :: Object3D -> RoofPlate -> PanelRenderer -> ButtonsRenderer -> PanelAPIInterpreter -> PanelLayer
defPanelLayerWith obj roof renderer btnsRenderer apiInterpreter = PanelLayer {
    object             : obj,
    renderer           : renderer,
    btnsRenderer       : btnsRenderer,
    apiInterpreter     : apiInterpreter,
    disposable         : pure unit,
    roof               : roof,
    arrayChanged       : empty,
    serverUpdated      : empty,
    arrayDragging      : pure false,
    plusDragging       : pure false,
    inactiveRoofTapped : empty,
    activeArray        : empty,
    currentPanels      : empty
}

_renderer :: forall t a r. Newtype t { renderer :: a | r } => Lens' t a
_renderer = _Newtype <<< prop (SProxy :: SProxy "renderer")

_btnsRenderer :: forall t a r. Newtype t { btnsRenderer :: a | r } => Lens' t a
_btnsRenderer = _Newtype <<< prop (SProxy :: SProxy "btnsRenderer")

_apiInterpreter :: forall t a r. Newtype t { apiInterpreter :: a | r } => Lens' t a
_apiInterpreter = _Newtype <<< prop (SProxy :: SProxy "apiInterpreter")

_arrayChanged :: forall t a r. Newtype t { arrayChanged :: a | r } => Lens' t a
_arrayChanged = _Newtype <<< prop (SProxy :: SProxy "arrayChanged")

_serverUpdated :: forall t a r. Newtype t {serverUpdated :: a | r } => Lens' t a
_serverUpdated = _Newtype <<< prop (SProxy :: SProxy "serverUpdated")

_arrayDragging :: forall t a r. Newtype t { arrayDragging :: a | r } => Lens' t a
_arrayDragging = _Newtype <<< prop (SProxy :: SProxy "arrayDragging")

_plusDragging :: forall t a r. Newtype t { plusDragging :: a | r } => Lens' t a
_plusDragging = _Newtype <<< prop (SProxy :: SProxy "plusDragging")

_inactiveRoofTapped :: forall t a r. Newtype t { inactiveRoofTapped :: a | r } => Lens' t a
_inactiveRoofTapped = _Newtype <<< prop (SProxy :: SProxy "inactiveRoofTapped")

_currentPanels :: forall t a r. Newtype t { currentPanels :: a | r } => Lens' t a
_currentPanels = _Newtype <<< prop (SProxy :: SProxy "currentPanels")

-- | internal panel layer state data structure
newtype PanelLayerState = PanelLayerState {
    object           :: Object3D,
    arrayConfig      :: ArrayConfig,
    editorMode       :: EditorMode,

    roofActive       :: Boolean,
    activeArray      :: Maybe Int,
    oldActiveArray   :: Maybe Int,  -- remember which array was active when the roof is deactivated

    layout           :: PanelsLayout,
    orientationToUse :: Maybe Orientation,

    initDragPos      :: Maybe Vector3,
    lastDragPos      :: Maybe Vector3,
    tempPanels       :: List Panel,

    panelOperations  :: List PanelOperation,
    arrayOperations  :: List ArrayOperation,
    btnsOperations   :: List ButtonOperation,
    arrayChanged     :: Maybe Unit
}

derive instance newtypePanelLayerState :: Newtype PanelLayerState _

mkState :: Object3D -> PanelLayerState
mkState obj = PanelLayerState {
        object           : obj,
        arrayConfig      : def,
        editorMode       : Showing,
        roofActive       : false,
        activeArray      : Nothing,
        oldActiveArray   : Nothing,
        layout           : def,
        orientationToUse : Nothing,
        initDragPos      : Nothing,
        lastDragPos      : Nothing,
        tempPanels       : Nil,
        panelOperations  : Nil,
        arrayOperations  : Nil,
        btnsOperations   : Nil,
        arrayChanged     : Nothing
    }

_activeArray :: forall t a r. Newtype t { activeArray :: a | r } => Lens' t a
_activeArray = _Newtype <<< prop (SProxy :: SProxy "activeArray")

_oldActiveArray :: forall t a r. Newtype t { oldActiveArray :: a | r } => Lens' t a
_oldActiveArray = _Newtype <<< prop (SProxy :: SProxy "oldActiveArray")

_layout :: forall t a r. Newtype t { layout :: a | r } => Lens' t a
_layout = _Newtype <<< prop (SProxy :: SProxy "layout")

_initDragPos :: forall t a r. Newtype t { initDragPos :: a | r } => Lens' t a
_initDragPos = _Newtype <<< prop (SProxy :: SProxy "initDragPos")

_lastDragPos :: forall t a r. Newtype t { lastDragPos :: a | r } => Lens' t a
_lastDragPos = _Newtype <<< prop (SProxy :: SProxy "lastDragPos")

_tempPanels :: forall t a r. Newtype t { tempPanels :: a | r } => Lens' t a
_tempPanels = _Newtype <<< prop (SProxy :: SProxy "tempPanels")

_orientationToUse :: forall t a r. Newtype t { orientationToUse :: a | r } => Lens' t a
_orientationToUse = _Newtype <<< prop (SProxy :: SProxy "orientationToUse")

_panelOperations :: forall t a r. Newtype t { panelOperations :: a | r } => Lens' t a
_panelOperations = _Newtype <<< prop (SProxy :: SProxy "panelOperations")

_arrayOperations :: forall t a r. Newtype t { arrayOperations :: a | r } => Lens' t a
_arrayOperations = _Newtype <<< prop (SProxy :: SProxy "arrayOperations")

_btnsOperations :: forall t a r. Newtype t { btnsOperations :: a | r } => Lens' t a
_btnsOperations = _Newtype <<< prop (SProxy :: SProxy "btnsOperations")


data PanelLayerOperation = POLoadPanels (List Panel)
                         | POAddPanel Panel
                         | PODeletePanel UUID
                         | POUpdateAlignment Alignment
                         | POUpdateOrientation Orientation
                         | PODragPanel (DragInfo Panel)
                         | PODragPlusBtn (DragInfo PlusButton)
                         | PORotRowInArr Int Int
                         | POUpdateArrayConfig ArrayConfig
                         | POActivateArray Int
                         | POActivateRoof Boolean
                         | POUpdateMode EditorMode

derive instance genericPanelLayerOperation :: Generic PanelLayerOperation _
instance showPanelLayerOperation :: Show PanelLayerOperation where
    show = genericShow

createPanelLayer :: PanelLayerConfig -> ArrayBuilder PanelLayer
createPanelLayer cfg = do
    layer <- liftEffect mkObject3D
    liftEffect $ setName "panel-layer" layer

    modeDyn <- view _editorMode <$> ask
    d <- liftEffect $ subscribeDyn (((/=) RoofEditing) <$> modeDyn) (flip setVisible layer)

    arrCfgDyn <- view _arrayConfig <$> ask
    apiCfg    <- view _apiConfig   <$> ask
    
    let roof = cfg ^. _roof

    { event: arrOpEvt, push: pushArrOpEvt }     <- liftEffect create
    { event: panelOpEvt, push: pushPanelOpEvt } <- liftEffect create
    { event: btnOpEvt, push: pushBtnOpEvt }     <- liftEffect create

    -- setup the panel renderer and button renderer
    panelRenderer <- setupPanelRenderer layer arrOpEvt (cfg ^. _opacity)
    btnsRenderer  <- liftRenderingM $ withRenderContext (view _rotateButtonMaterial) $ mkButtonsRenderer layer btnOpEvt
    -- setup the panel API interpreter
    let apiInterpreter = mkPanelAPIInterpreter $ def # _apiConfig  .~ apiCfg
                                                     # _roof       .~ roof
                                                     # _houseId    .~ (cfg ^. _houseId)
                                                     # _operations .~ panelOpEvt
        panelLayer = defPanelLayerWith layer roof panelRenderer btnsRenderer apiInterpreter

    Tuple nLayer stateEvt <- setupPanelLayer cfg panelLayer
    let pOpEvt     = fromFoldableE $ view _panelOperations <$> stateEvt
        arrayOpEvt = fromFoldableE $ view _arrayOperations <$> stateEvt
        btnOpsEvt  = fromFoldableE $ view _btnsOperations  <$> stateEvt
        arrChgEvt  = compact $ view _arrayChanged <$> stateEvt

        pArrOpEvt  = PanelOperation <$> pOpEvt

        -- current panels
        curPsEvt   = view (_layout <<< _panels) <$> stateEvt
    
    d1 <- liftEffect $ subscribe pOpEvt pushPanelOpEvt
    d2 <- liftEffect $ subscribe (arrayOpEvt <|> pArrOpEvt) pushArrOpEvt
    d3 <- liftEffect $ subscribe btnOpsEvt pushBtnOpEvt
    
    pure $ nLayer # _serverUpdated .~ apiInterpreter ^. _finished
                  # _arrayChanged  .~ arrChgEvt
                  # _disposable    %~ ((<*) (d *> d1 *> d2 *> d3))
                  # _currentPanels .~ curPsEvt
                  # _activeArray   .~ (getActiveArray <$> stateEvt)

setupPanelRenderer :: Object3D -> Event ArrayOperation -> Dynamic PanelOpacity -> ArrayBuilder PanelRenderer
setupPanelRenderer parent opEvt opacity = createPanelRenderer $ PanelRendererConfig {
        parent     : parent,
        operations : opEvt,
        opacity    : opacity
    }


setupPanelLayer :: PanelLayerConfig -> PanelLayer -> ArrayBuilder (Tuple PanelLayer (Event PanelLayerState))
setupPanelLayer cfg layer = do
    editorModeDyn <- view _editorMode <$> ask

    let roof            = cfg ^. _roof
        canEditDyn      = ((==) ArrayEditing) <$> editorModeDyn

        panelTapEvt     = multicast $ layer ^. _renderer <<< _tapped
        plusTapEvt      = layer ^. _btnsRenderer <<< _plusTapped

        roofActiveDyn   = cfg ^. _roofActive
        roofInactiveDyn = not <$> roofActiveDyn

        -- valid panel tap event when the roof is active
        panelTapped     = multicast $ gateDyn roofActiveDyn panelTapEvt

        newPanelEvt     = performEvent $ mkPanelAtPlusBtnPos roof <$> plusTapEvt

    -- add drag helper
    helper <- liftEffect mkDragHelper
    liftEffect $ add helper layer

    -- read the ArrayConfig event
    arrCfgEvt <- dynEvent <<< view _arrayConfig <$> ask

    { event: activeArrayEvt, push: pushActiveArray } <- liftEffect create

    { event: canDragPB, push: pushCanDragPB } <- liftEffect create
    let canDragPBDyn = step true canDragPB
    Tuple mlayer dragPBEvt <- liftEffect $ setupPlusBtnDragging layer helper canDragPBDyn

    let canDragArray = step true $ not <$> dynEvent (mlayer ^. _plusDragging)
    Tuple nlayer dragPanelEvt <- liftEffect $ setupPanelDragging mlayer helper canDragArray

    d <- liftEffect $ subscribeDyn (nlayer ^. _arrayDragging) (not >>> pushCanDragPB)

    let loadPanelEvt = POLoadPanels <$> cfg ^. _initPanels
        
        -- panel tap event might activate an array or delete the panel if the array is active
        actArrOrDelP p arr = if arrayNumber p == arr
                             then PODeletePanel (p ^. _uuid)
                             else POActivateArray $ arrayNumber p
        actArrOrDelPEvt = sampleOn activeArrayEvt (actArrOrDelP <$> panelTapped)

        addPanelEvt  = POAddPanel          <$> newPanelEvt
        rotRowEvt    = (\rb ->  PORotRowInArr (rb ^. _rowNumber) (rb ^. _arrayNumber)) <$> layer ^. _btnsRenderer <<< _rotTapped
        updArrCfgEvt = POUpdateArrayConfig <$> skip 1 arrCfgEvt
        actRoofEvt   = POActivateRoof      <$> dynEvent (cfg ^. _roofActive)
        updAlgnEvt   = POUpdateAlignment   <$> dynEvent (cfg ^. _alignment)
        updOrientEvt = POUpdateOrientation <$> dynEvent (cfg ^. _orientation)

        -- make sure all edit events only fired in ArrayEditing mode
        editOpEvt = gateDyn canEditDyn $ dragPBEvt <|> dragPanelEvt <|> actArrOrDelPEvt <|>
                                         addPanelEvt <|> rotRowEvt <|> updArrCfgEvt <|> actRoofEvt <|>
                                         updAlgnEvt <|> updOrientEvt

        updModeEvt = POUpdateMode <$> dynEvent editorModeDyn

        opEvt = editOpEvt <|> loadPanelEvt <|> updModeEvt

        defState = mkState $ layer ^. _object
        stateEvt = multicast $ foldEffect (flip (applyPanelLayerOp cfg)) opEvt defState

    -- push the current active array in state to activeArrayEvt
    d1 <- liftEffect $ subscribe (compact $ view _activeArray <$> stateEvt) pushActiveArray
        -- panel tapped when the roof is inactive, let's pipe the tap event to parent
    let resLayer = nlayer # _inactiveRoofTapped .~ multicast (const unit <$> gateDyn roofInactiveDyn panelTapEvt)
                          # _disposable %~ ((<*) (d *> d1))
    pure $ Tuple resLayer stateEvt

setupPanelDragging :: PanelLayer -> DraggableMesh -> Dynamic Boolean -> Effect (Tuple PanelLayer (Event PanelLayerOperation))
setupPanelDragging layer helper canDragArray = do
    let panelDragEvt = gateDyn canDragArray $ layer ^. _renderer <<< _dragged
        draggingPanelDyn = step Nothing $ (Just <<< view _object) <$> panelDragEvt

        -- let the drag events from drag helper take up the current dragging panel
        helperDragEvt = compact $ sampleDyn draggingPanelDyn $ (\d p -> flip mkDragInfo d <$> p) <$> helper ^. _dragged

        -- drag events from both panel and helper
        dEvt = multicast $ panelDragEvt <|> helperDragEvt

        dragStart = multicast $ filter isDragStart dEvt
        dragEnd = multicast $ debounce (Milliseconds 100.0) $ filter isDragEnd dEvt

        isDragging = step false $ (const true <$> dragStart) <|> (const false <$> dragEnd)

        dragEvt = gateDyn isDragging $ filter isDrag dEvt

    -- only enable the helper when user is actually dragging a panel
    d <- subscribeDyn isDragging (flip setVisible helper)

    let nLayer = layer # _arrayDragging .~ isDragging
                       # _disposable    %~ ((<*) d)

        evt = PODragPanel <$> (dragStart <|> dragEvt <|> dragEnd)
    pure $ Tuple nLayer evt


setupPlusBtnDragging :: PanelLayer -> DraggableMesh -> Dynamic Boolean -> Effect (Tuple PanelLayer (Event PanelLayerOperation))
setupPlusBtnDragging layer helper canDragPlus = do
    let pbDrag = multicast $ gateDyn canDragPlus $ layer ^. _btnsRenderer <<< _plusDragged
        draggingPbDyn = step Nothing $ (Just <<< view _object) <$> pbDrag

        -- let the drag events from drag helper take up the current dragging plus button
        helperDragEvt = compact $ sampleDyn draggingPbDyn $ (\d p -> flip mkDragInfo d <$> p) <$> helper ^. _dragged

        dEvt = multicast $ pbDrag <|> helperDragEvt

        dragStart = multicast $ filter isDragStart dEvt
        dragEnd = multicast $ debounce (Milliseconds 100.0) $ filter isDragEnd dEvt

        isDragging = step false $ (const true <$> dragStart) <|> (const false <$> dragEnd)
        dragEvt = gateDyn isDragging $ filter isDrag dEvt
    
    -- only enable the helper when user is actually dragging a panel
    d <- subscribeDyn isDragging (flip setVisible helper)

    let nLayer = layer # _plusDragging .~ isDragging
                       # _disposable   %~ ((<*) d)

        evt = PODragPlusBtn <$> (dragStart <|> dragEvt <|> dragEnd)
    pure $ Tuple nLayer evt

mkPanelAtPlusBtnPos :: RoofPlate -> PlusButton -> Effect Panel
mkPanelAtPlusBtnPos roof pb = do
    i <- genUUID
    pure $ def # _uuid        .~ i
               # _roofUUID    .~ roof ^. _id
               # _roofplateId .~ roof ^. _roofIntId
               # _arrNumber   .~ pb ^. _arrayNumber
               # _x           .~ pb ^. _x
               # _y           .~ pb ^. _y
               # _slope       .~ pb ^. _slope
               # _orientation .~ pb ^. _orientation

-- | function to update the ArrayLayout data structure 
updateLayout :: forall f. Foldable f => Functor f => PanelLayerConfig -> PanelLayerState -> f Panel -> Effect PanelsLayout
updateLayout lCfg st ps = if null ps
                          then getOrient >>= flip defaultLayout cfg
                          else layoutPanels ps cfg
    where cfg    = st ^. _arrayConfig
          getOrient = if cfg ^. _rackingType == BX  -- always use landscape for BX system
                        then pure Landscape
                        else case st ^. _orientationToUse of
                            Just o  -> pure o
                            Nothing -> current $ lCfg ^. _mainOrientation

applyPanelLayerOp :: PanelLayerConfig -> PanelLayerState -> PanelLayerOperation -> Effect PanelLayerState
applyPanelLayerOp cfg st (POLoadPanels ps)            = loadPanels cfg st ps
applyPanelLayerOp cfg st (POAddPanel p)               = addPanel cfg st p
applyPanelLayerOp cfg st (PODeletePanel pid)          = deletePanel cfg st pid
applyPanelLayerOp cfg st (POUpdateAlignment a)        = updateAlignment cfg st a
applyPanelLayerOp cfg st (POUpdateOrientation o)      = updateOrientation cfg st o
applyPanelLayerOp cfg st (PODragPanel d)              = dragPanel cfg st d
applyPanelLayerOp cfg st (PODragPlusBtn d)            = processDraggingPlus cfg st d
applyPanelLayerOp cfg st (PORotRowInArr r arr)        = rotateRowInArr cfg st r arr
applyPanelLayerOp cfg st (POUpdateArrayConfig arrCfg) = updateArrayConfig cfg st arrCfg
applyPanelLayerOp cfg st (POActivateArray arr)        = updateActiveArray cfg st arr
applyPanelLayerOp cfg st (POActivateRoof act)         = updateRoofActive cfg st act
applyPanelLayerOp cfg st (POUpdateMode m)             = updateEditorMode cfg st m

-- | get all panels in the current state
allPanels :: PanelLayerState -> List Panel
allPanels st = st ^. (_layout <<< _panels)

canEdit :: PanelLayerState -> Boolean
canEdit st = st ^. _editorMode == ArrayEditing

clearOperations :: PanelLayerState -> PanelLayerState
clearOperations st = st # _panelOperations .~ Nil
                        # _arrayOperations .~ Nil
                        # _btnsOperations  .~ Nil
                        # _arrayChanged    .~ Nothing


-- | make sure slope value of panels on non-flat roofs is set to zero
zeroSlope :: RoofPlate -> List Panel -> List Panel
zeroSlope roof ps | isFlat roof = ps
                  | otherwise   = set _slope def <$> ps

loadPanels :: PanelLayerConfig -> PanelLayerState -> List Panel -> Effect PanelLayerState
loadPanels cfg st Nil = pure st
loadPanels cfg st ps  = do
    let nps = zeroSlope (cfg ^. _roof) ps
    newLayout <- updateLayout cfg st nps
    pure $ st # _layout .~ newLayout
              # _panelOperations .~ singleton (LoadPanels nps)

-- | add a new panel to the current state
addPanel :: PanelLayerConfig -> PanelLayerState -> Panel -> Effect PanelLayerState
addPanel cfg st p = do
    let oldPs = allPanels st
        -- use current layout's array alignment as default alignment value of the new panel
        algn  = maybe Grid (view _alignment) $ getArrayAt (p ^. _arrNumber) (st ^. _layout)
        np    = p # _alignment .~ algn
        ps    = np : oldPs

    -- update layout with new panels
    newLayout <- updateLayout cfg st ps

    -- get the updated new panel
    let updatedPs = newLayout ^. _panelsUpdated
        newP      = fromMaybe p $ get (p ^. _uuid) updatedPs
        newUpdPs  = delete (p ^. _uuid) updatedPs

        addNewPOp = AddPanel newP
        updOps    = UpdatePanels $ toUnfoldable newUpdPs

    checkAndUpdateBtnOps cfg true $ (clearOperations st) # _panelOperations .~ addNewPOp : updOps : Nil
                                                         # _layout          .~ newLayout
                                                         # _arrayChanged    .~ Just unit

addPanels :: PanelLayerConfig -> PanelLayerState -> List Panel -> Effect PanelLayerState
addPanels cfg st ps = do
    let oldPs = allPanels st
        newPs = append ps oldPs

    -- update layout with new panels
    newLayout <- updateLayout cfg st newPs

    let updPs = newLayout ^. _panelsUpdated
        f p = case get (p ^. _uuid) updPs of
                    Just np -> np
                    Nothing -> p
        nps = f <$> ps
        updatedPs = deletePanels (view _uuid <$> nps) updPs

        newPsOp = AddPanels nps
        updOps  = UpdatePanels $ toUnfoldable updatedPs
    
    checkAndUpdateBtnOps cfg true $ (clearOperations st) # _panelOperations .~ newPsOp : updOps : Nil
                                                         # _layout          .~ newLayout
                                                         # _arrayChanged    .~ Just unit

deletePanel :: PanelLayerConfig -> PanelLayerState -> UUID -> Effect PanelLayerState
deletePanel cfg st pid = do
    let ps = filter ((/=) pid <<< view _uuid) $ allPanels st
    
    newLayout <- updateLayout cfg st ps
    let delPOp = DelPanel pid
        updOps = UpdatePanels $ toUnfoldable (newLayout ^. _panelsUpdated)
    
    checkAndUpdateBtnOps cfg true $ (clearOperations st) # _panelOperations .~ delPOp : updOps : Nil
                                                         # _layout          .~ newLayout
                                                         # _arrayChanged    .~ Just unit


-- | get the current active PanelArray value
getActiveArray :: PanelLayerState -> Maybe PanelArray
getActiveArray st = join $ flip lookup (st ^. _layout <<< _arrays) <$> st ^. _activeArray

updateAlignment :: PanelLayerConfig -> PanelLayerState -> Alignment -> Effect PanelLayerState
updateAlignment cfg st algn = case filter ((/=) algn <<< view _alignment) $ getActiveArray st of
    Nothing -> pure $ clearOperations st
    Just arr -> do
        let rows    = fromFoldable $ arr ^. _rows
            aligned = alignPanelRows algn rows
            layout  = st ^. _layout

            -- check if there're panels touching other arrays
            touched = panelsTouchingOtherArray layout aligned
            toDel   = touched.yes
            toUpd   = touched.no

            -- panels from other arrays
            psOtherArrs = filter ((/=) (arr ^. _arrayNumber) <<< arrayNumber) $ allPanels st

        -- update layout with all updated panels
        newLayout <- updateLayout cfg st $ append toUpd psOtherArrs

        -- combine all panels that have been changed
        let updated = toUnfoldable $ merge (newLayout ^. _panelsUpdated) (UpdatedPanels.fromFoldable toUpd)

            toDelOp = if null toDel then Nil else singleton $ DelPanels (view _uuid <$> toDel)
            toUpdOp = UpdatePanels updated

        checkAndUpdateBtnOps cfg false $ (clearOperations st) # _panelOperations .~ toUpdOp : toDelOp
                                                              # _layout          .~ newLayout
                                                              # _arrayChanged    .~ Just unit


updateOrientation :: PanelLayerConfig -> PanelLayerState -> Orientation -> Effect PanelLayerState
updateOrientation cfg st o = do
    let nst = (clearOperations st) # _orientationToUse .~ Just o
    newLayout <- updateLayout cfg nst Nil
    checkAndUpdateBtnOps cfg true $ nst # _panelOperations .~ singleton DeleteAll
                                        # _layout          .~ newLayout
                                        # _arrayChanged    .~ Just unit
                                        # _orientationToUse .~ Nothing


dragPanel :: PanelLayerConfig -> PanelLayerState -> DragInfo Panel -> Effect PanelLayerState
dragPanel cfg st d = case d ^. _dragType of
    DragStart -> startDragging cfg st d
    Drag      -> drag cfg st d
    DragEnd   -> endDragging cfg st (d ^. _object <<< _arrNumber)

startDragging :: PanelLayerConfig -> PanelLayerState -> DragInfo Panel -> Effect PanelLayerState
startDragging cfg st d = if not (st ^. _roofActive) || st ^. _activeArray /= Just (d ^. _object <<< _arrNumber)
                         then pure $ clearOperations st
                         else do
                            p <- worldToLocal (d ^. _point) (st ^. _object)
                            -- find all panels in the dragged array and put it in tempPanels field
                            let ps         = allPanels st
                                arrNum     = d ^. _object <<< _arrNumber
                                draggingPs = filter ((==) arrNum <<< arrayNumber) ps
                            pure $ (clearOperations st) # _lastDragPos    .~ Just p
                                                        # _arrayChanged   .~ Just unit
                                                        # _tempPanels     .~ draggingPs
                                                        # _btnsOperations .~ singleton ResetButtons


drag :: PanelLayerConfig -> PanelLayerState -> DragInfo Panel -> Effect PanelLayerState
drag cfg st d = if not (st ^. _roofActive) || st ^. _activeArray /= Just (d ^. _object <<< _arrNumber)
                then pure st
                else do
                    p <- worldToLocal (d ^. _point) (st ^. _object)
                    let delta = maybe def (p <-> _) (st ^. _lastDragPos)
                        arrNum = d ^. _object <<< _arrNumber
                    pure $ (clearOperations st) # _lastDragPos     .~ Just p
                                                # _arrayOperations .~ singleton (MoveArray arrNum delta)
                                                # _tempPanels      %~ map (addDelta delta)

endDragging :: PanelLayerConfig -> PanelLayerState -> Int -> Effect PanelLayerState
endDragging cfg st arr = if not (st ^. _roofActive) || st ^. _activeArray /= Just arr || null (st ^. _tempPanels)
    then checkAndUpdateBtnOps cfg false $ (clearOperations st) # _lastDragPos  .~ Nothing
                                                               # _tempPanels   .~ Nil
    else do
        let -- get panels in other arrays that're not dragged
            allPs      = allPanels st
            otherArrPs = filter ((/=) arr <<< arrayNumber) allPs

            -- check if the current array panels is outside of the roof or touching other arrays
            outside = panelsOutsideRoof cfg (st ^. _tempPanels)
            touched = panelsTouchingOtherArray (st ^. _layout) outside.no
            -- delete panels outside roof or touching other arrays
            toDel   = append outside.yes touched.yes
        -- update layout with left panels and panels in other arrays
        newLayout <- updateLayout cfg st $ append otherArrPs touched.no
        -- merge all updated panels
        let updated = merge (newLayout ^. _panelsUpdated) (UpdatePanels.fromFoldable touched.no)

            delOp = DelPanels (view _uuid <$> toDel)
            updOp = UpdatePanels (toUnfoldable updated)
        checkAndUpdateBtnOps cfg true $ (clearOperations st) # _panelOperations .~ delOp : updOp : Nil
                                                             # _layout          .~ newLayout
                                                             # _lastDragPos     .~ Nothing
                                                             # _arrayChanged    .~ Just unit
                                                             # _tempPanels      .~ Nil


processDraggingPlus :: PanelLayerConfig -> PanelLayerState -> DragInfo PlusButton -> Effect PanelLayerState
processDraggingPlus cfg st d = if null (allPanels st)
    then pure $ clearOperations st
    else do
        p <- worldToLocal (d ^. _point) (st ^. _object)
        case d ^. _dragType of
            DragStart -> do
                let pb    = d ^. _object
                    pbPos = mkVec3 (meterVal $ pb ^. _x) (meterVal $ pb ^. _y) 0.0
                pure $ (clearOperations st) # _lastDragPos    .~ Just p
                                            # _initDragPos    .~ Just pbPos
                                            # _btnsOperations .~ singleton (HideButtonsExcept $ d ^. _object)
            Drag -> do
                let delta   = maybe def (p <-> _) (st ^. _lastDragPos)
                    panel   = unsafePartial $ head $ allPanels st
                    initPos = fromMaybe def $ st ^. _initDragPos
                tempPs <- tempPanels (st ^. _arrayConfig) (st ^. _layout <<< _tree) panel initPos p
                pure $ (clearOperations st) # _btnsOperations  .~ singleton (MovePlusButton (d ^. _object) delta)
                                            # _arrayOperations .~ singleton (TempPanels tempPs)
                                            # _lastDragPos     .~ Just p
                                            # _tempPanels      .~ tempPs
            DragEnd -> do
                let tmpPs = st ^. _tempPanels
                nst <- addPanels cfg st tmpPs
                checkAndUpdateBtnOps cfg true $ nst # _btnsOperations  .~ singleton ResetButtons
                                                    # _arrayOperations .~ singleton PreserveTempPanels
                                                    # _tempPanels      .~ Nil
                                                    # _initDragPos     .~ Nothing
                                                    # _lastDragPos     .~ Nothing
                                                    # _arrayChanged    .~ Just unit


rotateRowInArr :: PanelLayerConfig -> PanelLayerState -> Int -> Int -> Effect PanelLayerState
rotateRowInArr cfg st row arr = case getArrayAt arr (st ^. _layout) of
    Nothing    -> pure $ clearOperations st
    Just array -> do
        let Tuple allPs updatedPs = rotateRow array row
            outside    = panelsOutsideRoof cfg allPs
            touched    = panelsTouchingOtherArray (st ^. _layout) outside.no
            toDel      = append outside.yes touched.yes
            toDelIds   = view _uuid <$> toDel

            otherArrPs = filter ((/=) arr <<< arrayNumber) $ allPanels st
        -- update layout with new valid panels and panels from other arrays
        newLayout <- updateLayout cfg st (append touched.no otherArrPs)
        let updated = merge (newLayout ^. _panelsUpdated) (deletePanels toDelIds $ UpdatedPanels.fromFoldable updatedPs)

            delOp = DelPanels toDelIds
            updOp = UpdatePanels $ toUnfoldable updated

        checkAndUpdateBtnOps cfg true $ (clearOperations st) # _panelOperations .~ delOp : updOp : Nil
                                                             # _layout          .~ newLayout
                                                             # _arrayChanged    .~ Just unit

updateArrayConfig :: PanelLayerConfig -> PanelLayerState -> ArrayConfig -> Effect PanelLayerState
updateArrayConfig cfg st arrCfg = do
    let rt = arrCfg ^. _rackingType
    if rt == BX || rt == XRFlat
    then do
        let nst = if rt == BX
                  then (clearOperations st) # _orientationToUse .~ Just Landscape
                  else clearOperations st
        newLayout <- updateLayout cfg nst Nil
        checkAndUpdateBtnOps cfg true $ nst # _panelOperations .~ singleton DeleteAll
                                            # _layout          .~ newLayout
                                            # _arrayConfig     .~ arrCfg
                                            # _arrayChanged    .~ Just unit
                                            # _orientationToUse .~ Nothing
    else pure $ (clearOperations st) # _arrayConfig .~ arrCfg


updateActiveArray :: PanelLayerConfig -> PanelLayerState -> Int -> Effect PanelLayerState
updateActiveArray cfg st arr = checkAndUpdateBtnOps cfg false $ (clearOperations st) # _btnsOperations .~ singleton ResetButtons
                                                                                     # _activeArray    .~ Just arr

updateRoofActive :: PanelLayerConfig -> PanelLayerState -> Boolean -> Effect PanelLayerState
updateRoofActive cfg st active = if active
    then do let layout = st ^. _layout
                arr = case st ^. _oldActiveArray of
                         Just oa -> if oa >= 0 && oa < size (layout ^. _arrays)
                                    then oa
                                    else findActiveArray layout
                         Nothing -> findActiveArray layout
                nst = st # _roofActive .~ active
            updateActiveArray cfg nst arr
    else pure $ (clearOperations st) # _oldActiveArray .~ st ^. _activeArray
                                     # _activeArray    .~ Nothing
                                     # _roofActive     .~ active
                                     # _btnsOperations .~ singleton ResetButtons

-- update the editor mode. reset all buttons in showing mode and add buttons for active array in arrayediting mode
updateEditorMode :: PanelLayerConfig -> PanelLayerState -> EditorMode -> Effect PanelLayerState
updateEditorMode cfg st Showing      = pure $ (clearOperations st) # _btnsOperations .~ singleton ResetButtons
updateEditorMode cfg st ArrayEditing = checkAndUpdateBtnOps cfg false $ clearOperations st
updateEditorMode cfg st RoofEditing  = pure $ clearOperations st
updateEditorMode cfg st HouseBuilding = pure $ clearOperations st

checkAndUpdateBtnOps :: PanelLayerConfig -> Boolean -> PanelLayerState -> Effect PanelLayerState
checkAndUpdateBtnOps cfg arrayChanged st = if st ^. _roofActive
    then do
        let layout    = st ^. _layout
            actArr    = fromMaybe 1000000 $ st ^. _activeArray
            arrCnt    = size $ layout ^. _arrays
            newActArr = if arrayChanged && actArr >= arrCnt then findActiveArray layout else actArr
        
        -- if there's no arrays, then create a temporary layout which will have a single array
        -- that use the current main orientation
        l <- if arrCnt == 0
             then updateLayout cfg st Nil
             else pure layout

        -- get the active array and update buttons for it
        case getArrayAt newActArr l of
            Just a -> do btnOps <- btnOpsForArray cfg st a
                         pure $ st # _btnsOperations %~ flip append btnOps
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


-- | get panels that has touched other arrays in a list of panels
panelsTouchingOtherArray :: PanelsLayout -> List Panel -> { no :: List Panel, yes :: List Panel }
panelsTouchingOtherArray layout = partition f
    where f p = any ((/=) (arrayNumber p) <<< arrayNumber) $ neighbors layout p


panelsOutsideRoof :: PanelLayerConfig -> List Panel -> { no :: List Panel, yes :: List Panel }
panelsOutsideRoof cfg = partition f
    where f p = not $ wrapAroundPoints (cfg ^. _roof) (panelVertices p)


-- setup a helper layer to make dragging panels/plus buttons easier
mkDragHelper :: Effect DraggableMesh
mkDragHelper = do
    geo <- mkBoxGeometry 1000.0 1000.0 0.02
    mat <- mkMeshBasicMaterial 0xffffff
    setTransparent true mat
    setOpacity 0.01 mat

    m <- mkDraggableMesh geo mat
    setName "drag-helper" m
    setCastShadow false m
    setRenderOrder 30 m
    setVisible false m

    pure m
