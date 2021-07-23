module SmartHouse.HouseBuilder (buildHouse, HouseBuilderConfig(..), HouseBuilt(..),  _hasHouse) where

import Prelude hiding (degree)

import API (API, APIConfig, runAPI)
import API.Image (ImageResp, _link, _pixelPerMeter, getImageMeta)
import API.Panel (loadPanels)
import API.Roofplate (loadRoofplates)
import API.SmartHouse (SavingStep(..), createManual, isFinished, repeatCheckUntilReady, savedHouseId, uploadMeshFiles)
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Custom.Mesh (TapMouseMesh)
import Data.Array (fromFoldable)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as M
import Data.Maybe (Maybe(..), isNothing)
import Data.Meter (meterVal)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.UUID (UUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_apiConfig, _buttons, _height, _houseId, _leadId, _modeDyn, _mouseMove, _name, _panelType, _panels, _parent, _roofs, _shadeSelected, _slope, _tapped, _textureInfo, _updated, _width)
import Editor.Editor (Editor, _sizeDyn, setMode)
import Editor.EditorMode as EditorMode
import Editor.HouseEditor (ArrayEditParam, HouseConfig, _dataServer, _heatmapTexture, _rotBtnTexture)
import Editor.RoofManager (RoofsData)
import Editor.SceneEvent (size)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, dynEvent, gateDyn, performDynamic, sampleDyn, step)
import FRP.Event (Event, create, keepLatest, sampleOn, sampleOn_, subscribe)
import FRP.Event.Extra (delay, multicast, performEvent)
import Math.Angle (Angle, degree)
import Model.ActiveMode (ActiveMode(..), fromBoolean, isActive)
import Model.Hardware.PanelTextureInfo (PanelTextureInfo)
import Model.Hardware.PanelType (PanelType(..))
import Model.SmartHouse.House (House, JSHouses(..), _trees, createHouseFrom, exportHouse)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _imageFile, _size, _texture, mkHouseTextureInfo)
import Model.SmartHouse.Tree (Tree, TreeNode, TreeOp(..))
import Model.UUID (idLens)
import Models.SmartHouse.ActiveItem (ActHouseRoof, ActiveItem(..))
import OBJExporter (MeshFiles, exportObject)
import Rendering.DynamicNode (eventNode)
import Rendering.Node (Node, _exportable, fixNodeDWith, fixNodeE, fixNodeEWith, getEnv, getParent, localEnv, mkNodeEnv, node, runNode, tapMouseMesh)
import Rendering.TextureLoader (textureFromUrl)
import SmartHouse.ActiveItemUI (_deleteHouse)
import SmartHouse.HouseEditor (HouseRenderMode(..), _arrayEditParam, _house, _roofsData, editHouse, renderHouse)
import SmartHouse.HouseTracer (TracerMode(..), _stopTracing, _tracedPolygon, _tracerMode, _undoTracing, traceHouse)
import SmartHouse.ShadeOption (ShadeOption)
import SmartHouse.TreeBuilder (buildTree, editTree)
import SmartHouse.UI (_activeItemDyn, _savingStepDyn, houseBuilderUI)
import Smarthouse.HouseNode (HouseNode, HouseOp(..), _actHouseRoof, _activated)
import Specular.Dom.Widget (runMainWidgetInNode)
import Three.Core.Geometry (mkPlaneGeometry)
import Three.Core.Material (mkMeshBasicMaterialWithTexture)
import Three.Loader.TextureLoader (clampToEdgeWrapping, repeatWrapping, setRepeat, setWrapS, setWrapT, textureHeight, textureImageEvt, textureWidth)
import Type.Proxy (Proxy(..))
import UI.ButtonPane (_close, _reset, _save, _showResetDyn, _showSaveDyn, _undo)
import UI.EditPane (_buildTree)
import UI.EditorUIOp (EditorUIOp(..))
import UI.RoofEditorUI (_editorOp)
import Unsafe.Coerce (unsafeCoerce)
import Util (foldEvtWith)
import Web.File (File)

-- NOTE: global value to toggle between rendering house as full 3D editor or 2D wireframes
houseRenderMode :: HouseRenderMode
houseRenderMode = EditHouseMode


newtype HouseBuilderConfig = HouseBuilderConfig {
    leadId         :: Int,

    -- fields used to construct HouseConfig when run the array editor in house builder
    dataServer     :: String,
    textureInfo    :: PanelTextureInfo,
    rotBtnTexture  :: String,
    heatmapTexture :: String,
    panelType      :: Dynamic PanelType,
    apiConfig      :: APIConfig
}

derive instance Newtype HouseBuilderConfig _
instance Default HouseBuilderConfig where
    def = HouseBuilderConfig {
        leadId         : 0,

        dataServer     : "",
        textureInfo    : def,
        rotBtnTexture  : "",
        heatmapTexture : "",
        panelType      : pure Premium,
        apiConfig      : def
        }

newtype HouseBuilt = HouseBuilt {
    hasHouse    :: Event Boolean,
    tracerMode  :: Event TracerMode,
    saveStepEvt :: Event SavingStep,
    editorOp    :: Event EditorUIOp,
    activeItem  :: Event (Maybe ActiveItem)
    }

derive instance Newtype HouseBuilt _
instance Default HouseBuilt where
    def = HouseBuilt {
        hasHouse    : empty,
        tracerMode  : empty,
        saveStepEvt : empty,
        editorOp    : empty,
        activeItem  : empty
        }

_hasHouse :: forall t a r. Newtype t { hasHouse :: a | r } => Lens' t a
_hasHouse = _Newtype <<< prop (Proxy :: Proxy "hasHouse")

_saveStepEvt :: forall t a r. Newtype t { saveStepEvt :: a | r } => Lens' t a
_saveStepEvt = _Newtype <<< prop (Proxy :: Proxy "saveStepEvt")

_activeItem :: forall t a r. Newtype t { activeItem :: a | r } => Lens' t a
_activeItem = _Newtype <<< prop (Proxy :: Proxy "activeItem")

compactHouseBuilt :: Event HouseBuilt -> HouseBuilt
compactHouseBuilt e = def # _hasHouse    .~ keepLatest (view _hasHouse    <$> e)
                          # _tracerMode  .~ keepLatest (view _tracerMode  <$> e)
                          # _saveStepEvt .~ keepLatest (view _saveStepEvt <$> e)
                          # _editorOp    .~ keepLatest (view _editorOp    <$> e)
                          # _activeItem  .~ keepLatest (view _activeItem  <$> e)


loadHouseTexture :: ImageResp -> Event HouseTextureInfo
loadHouseTexture img = performEvent $ f <$> textureFromUrl (img ^. _link)
    where f t = do
              setWrapS clampToEdgeWrapping t
              setWrapT repeatWrapping t
              setRepeat 1.0 1.0 t

              let s = size (textureWidth t) (textureHeight t)
                  d = textureImageEvt t
              pure $ mkHouseTextureInfo t s d (img ^. _pixelPerMeter)


mkHelperPlane :: forall e. HouseTextureInfo -> Node e TapMouseMesh
mkHelperPlane t = do
    let w = meterVal $ t ^. _size <<< _width
        h = meterVal $ t ^. _size <<< _height
    geo <- liftEffect $ mkPlaneGeometry w h 10 10
    mat <- liftEffect $ mkMeshBasicMaterialWithTexture $ t ^. _texture

    tapMouseMesh (def # _name       .~ "helper-plane"
                      # _exportable .~ true) geo mat

-- internal data structure to manage houses and trees
type HouseDict = UUIDMap House
type TreeDict = UUIDMap Tree

newtype HouseDictData = HouseDictData {
    houses         :: HouseDict,
    trees          :: TreeDict,
    housesToRender :: Maybe HouseDict,
    treesToRender  :: Maybe TreeDict
    }

derive instance Newtype HouseDictData _
derive instance Generic HouseDictData _
instance Show HouseDictData where
    show = genericShow
instance Default HouseDictData where
    def = HouseDictData {
        houses         : M.empty,
        trees          : M.empty,
        housesToRender : Nothing,
        treesToRender  : Nothing
        }

_houses :: forall t a r. Newtype t { houses :: a | r } => Lens' t a
_houses = _Newtype <<< prop (Proxy :: Proxy "houses")

_housesToRender :: forall t a r. Newtype t { housesToRender :: a | r } => Lens' t a
_housesToRender = _Newtype <<< prop (Proxy :: Proxy "housesToRender")

_treesToRender :: forall t a r. Newtype t { treesToRender :: a | r } => Lens' t a
_treesToRender = _Newtype <<< prop (Proxy :: Proxy "treesToRender")


hasHouse :: HouseDictData -> Boolean
hasHouse = not <<< M.isEmpty <<< view _houses

-- mark all houses to be rendered
renderAllHouses :: HouseDictData -> HouseDictData
renderAllHouses s = s # _housesToRender .~ Just (s ^. _houses)

-- mark all trees to be rendered
renderAllTrees :: HouseDictData -> HouseDictData
renderAllTrees s = s # _treesToRender .~ Just (s ^. _trees)

-- | update the HouseDictData with house operations
applyHouseOp :: HouseOp -> HouseDictData -> HouseDictData
applyHouseOp (HouseOpCreate house) d = renderAllHouses $ d # _houses %~ M.insert (house ^. idLens) house
applyHouseOp (HouseOpDelete hid)   d = if M.member hid (d ^. _houses)
                                       then renderAllHouses $ d # _houses %~ M.delete hid
                                       else d # _housesToRender .~ Nothing
applyHouseOp (HouseOpUpdate house) d = d # _houses %~ M.insert (house ^. idLens) house
                                         # _housesToRender .~ Nothing


-- | Update the HosueDictData with Tree operations
applyTreeOp :: TreeOp -> HouseDictData -> HouseDictData
applyTreeOp (TreeOpCreate tree) d = renderAllTrees $ d # _trees %~ M.insert (tree ^. idLens) tree
applyTreeOp (TreeOpDelete tid)  d = if M.member tid (d ^. _trees)
                                    then renderAllTrees $ d # _trees %~ M.delete tid
                                    else d # _treesToRender .~ Nothing
applyTreeOp (TreeOpUpdate tree) d = d # _trees %~ M.insert (tree ^. idLens) tree
                                      # _treesToRender .~ Nothing


exportHouses :: HouseDictData -> JSHouses
exportHouses hd = JSHouses { houses : fromFoldable $ M.values $ exportHouse <$> hd ^. _houses }

-- get house update event from a list of house nodes
getHouseUpd :: forall f. Foldable f => Functor f => f HouseNode -> Event HouseOp
getHouseUpd = foldEvtWith (view _updated)

getActiveRoof :: forall f. Foldable f => Functor f => f HouseNode -> Event ActHouseRoof
getActiveRoof = foldEvtWith (view _actHouseRoof)

-- get the activated house from a list of house nodes
getActivated :: forall f. Foldable f => Functor f => f HouseNode -> Event UUID
getActivated = foldEvtWith f
    where f n = const (n ^. idLens) <$> n ^. _activated

getTappedTree :: forall f. Foldable f => Functor f => f TreeNode -> Event UUID
getTappedTree = foldEvtWith f
    where f n = const (n ^. idLens) <$> n ^. _tapped

getActiveTree :: UUID -> HouseDictData -> Maybe Tree
getActiveTree tid hd = M.lookup tid (hd ^. _trees)

getTreeUpd :: forall f. Foldable f => Functor f => f TreeNode -> Event TreeOp
getTreeUpd = foldEvtWith (view _updated)

renderHouseDict :: Dynamic (Maybe UUID) -> HouseConfig -> ArrayEditParam -> Event ShadeOption -> Event RoofsData -> HouseDict -> Node HouseTextureInfo (UUIDMap HouseNode)
renderHouseDict actIdDyn houseCfg arrParam shadeEvt roofsDatEvt houses = traverse render houses
    where getMode h (Just i) | h ^. idLens == i = Active
                             | otherwise        = Inactive
          getMode _ Nothing                     = Inactive
          
          render h = if houseRenderMode == EditHouseMode
                     then do
                         let md = getMode h <$> actIdDyn
                         editHouse houseCfg $ def # _modeDyn        .~ md
                                                  # _house          .~ h
                                                  # _shadeSelected  .~ gateDyn (isActive <$> md) shadeEvt
                                                  # _roofsData      .~ roofsDatEvt
                                                  # _arrayEditParam .~ arrParam
                     else renderHouse h


renderTrees :: forall e. Dynamic (Maybe UUID) -> TreeDict -> Node e (UUIDMap TreeNode)
renderTrees actIdDyn trees = traverse render trees
    where getMode t (Just i) | t ^. idLens == i = Active
                             | otherwise        = Inactive
          getMode _ Nothing                     = Inactive
          
          render t = editTree t (getMode t <$> actIdDyn)


tracerMode :: Maybe UUID -> Boolean -> ActiveMode
tracerMode h t = fromBoolean $ isNothing h && not t

treeBuilderMode :: Maybe UUID -> Boolean -> ActiveMode
treeBuilderMode h t = fromBoolean $ isNothing h && t

newtype BuilderInputEvts = BuilderInputEvts {
    export        :: Event Unit,
    undoTracing   :: Event Unit,
    stopTracing   :: Event Unit,
    shadeSelected :: Event ShadeOption,
    slope         :: Event Angle,
    deleteHouse   :: Event Unit,
    buildTree     :: Event Boolean
    }

derive instance Newtype BuilderInputEvts _
instance Default BuilderInputEvts where
    def = BuilderInputEvts {
        export        : empty,
        undoTracing   : empty,
        stopTracing   : empty,
        shadeSelected : empty,
        slope         : empty,
        deleteHouse   : empty,
        buildTree     : empty
        }

_export :: forall t a r. Newtype t { export :: a | r } => Lens' t a
_export = _Newtype <<< prop (Proxy :: Proxy "export")


createHouseBuilder :: BuilderInputEvts -> Node HouseBuilderConfig HouseBuilt
createHouseBuilder evts = node (def # _name .~ "house-builder") $ do
    cfg <- getEnv
    let lId = cfg ^. _leadId

    -- load image meta info
    imgEvt <- liftEffect $ runAPI (getImageMeta $ def # _leadId .~ lId) (cfg ^. _apiConfig)

    -- load texture image
    let tInfoEvt = keepLatest $ loadHouseTexture <$> imgEvt
    evt <- eventNode $ builderForHouse evts <$> tInfoEvt
    pure $ compactHouseBuilt evt


houseCfgFromBuilderCfg :: HouseBuilderConfig -> HouseConfig
houseCfgFromBuilderCfg cfg = def # _dataServer     .~ (cfg ^. _dataServer)
                                 # _modeDyn        .~ pure EditorMode.ArrayEditing
                                 # _textureInfo    .~ (cfg ^. _textureInfo)
                                 # _rotBtnTexture  .~ (cfg ^. _rotBtnTexture)
                                 # _heatmapTexture .~ (cfg ^. _heatmapTexture)
                                 # _panelType      .~ (cfg ^. _panelType)
                                 # _apiConfig      .~ (cfg ^. _apiConfig)

builderForHouse :: BuilderInputEvts -> HouseTextureInfo -> Node HouseBuilderConfig HouseBuilt
builderForHouse evts tInfo =
    fixNodeEWith def \hdEvt ->
        fixNodeDWith Nothing \actIdDyn ->
                fixNodeE \roofsDatEvt -> do
                    pNode <- getParent
                    cfg   <- getEnv
                    
                    -- add helper plane that accepts tap and drag events
                    helper <- mkHelperPlane tInfo

                    -- render all houses
                    let houseToRenderEvt = compact $ view _housesToRender <$> hdEvt
                        treesToRenderEvt = compact $ view _treesToRender <$> hdEvt
                        houseCfg = houseCfgFromBuilderCfg cfg

                        shadeEvt = evts ^. _shadeSelected
                        arrParam = def
                    
                    -- render houses and trees
                    nodesEvt <- localEnv (const tInfo) $ eventNode (renderHouseDict actIdDyn houseCfg arrParam shadeEvt roofsDatEvt <$> houseToRenderEvt)
                    treesEvt <- eventNode (renderTrees actIdDyn <$> treesToRenderEvt)

                    let deactEvt    = multicast $ const Nothing <$> helper ^. _tapped

                        actEvt      = keepLatest $ getActivated <$> nodesEvt
                        actRoofEvt  = Just <$> keepLatest (map ActiveHouse <<< getActiveRoof <$> nodesEvt)

                        treeTapEvt = multicast $ keepLatest $ getTappedTree <$> treesEvt
                        actTreeEvt = map ActiveTree <$> (sampleOn hdEvt $ getActiveTree <$> treeTapEvt)

                        newActIdEvt = (Just <$> (actEvt <|> treeTapEvt)) <|> deactEvt

                        mouseEvt = multicast $ helper ^. _mouseMove
                        delEvt   = multicast $ evts ^. _deleteHouse

                        buildTreeDyn = step false $ evts ^. _buildTree

                    -- trace new house
                    traceRes <- traceHouse $ def # _modeDyn     .~ (tracerMode <$> actIdDyn <*> buildTreeDyn)
                                                 # _mouseMove   .~ mouseEvt
                                                 # _undoTracing .~ (evts ^. _undoTracing)
                                                 # _stopTracing .~ (evts ^. _stopTracing)

                    newTreeEvt <- buildTree (treeBuilderMode <$> actIdDyn <*> buildTreeDyn) mouseEvt

                    -- create house from the traced polygon
                    let slopeDyn  = step (degree 30.0) (evts ^. _slope)
                        polyDyn   = step Nothing $ Just <$> traceRes ^. _tracedPolygon
                        mkHouse s = traverse (createHouseFrom s)
                        houseDyn  = performDynamic $ mkHouse <$> slopeDyn <*> polyDyn
                        houseEvt  = compact $ dynEvent houseDyn

                        addHouseEvt = HouseOpCreate <$> houseEvt
                        updHouseEvt = keepLatest (getHouseUpd <$> nodesEvt)

                        mkDelOp _ (Just h) = Just $ HouseOpDelete h
                        mkDelOp _ Nothing  = Nothing
                        
                        delHouseEvt = multicast $ compact $ sampleDyn actIdDyn $ mkDelOp <$> delEvt

                        opEvt = addHouseEvt <|> updHouseEvt <|> delHouseEvt

                        -- tree operations
                        mkDelTreeOp _ (Just t) = Just $ TreeOpDelete t
                        mkDelTreeOp _ Nothing  = Nothing

                        addTreeEvt = TreeOpCreate <$> newTreeEvt
                        delTreeEvt = multicast $ compact $ sampleDyn actIdDyn $ mkDelTreeOp <$> delEvt
                        updTreeEvt = keepLatest $ getTreeUpd <$> treesEvt

                        treeOpEvt = addTreeEvt <|> updTreeEvt <|> delTreeEvt

                        -- update HouseDictData by applying the house operations
                        newHdEvt = multicast $ sampleOn hdEvt $ (applyHouseOp <$> opEvt) <|>
                                                                (applyTreeOp <$> treeOpEvt)

                        exportEvt = multicast $ evts ^. _export
                    
                        toExpEvt = delay 15 exportEvt
                    
                        meshFilesEvt = performEvent $ const (exportObject pNode) <$> toExpEvt
                        housesEvt    = exportHouses <$> sampleOn_ hdEvt toExpEvt

                        stepEvt = multicast $ saveMeshes cfg (tInfo ^. _imageFile) meshFilesEvt housesEvt

                        -- get the new generated house id after saving finished
                        finishSavingEvt = multicast $ filter isFinished stepEvt
                        newHouseIdEvt = compact $ savedHouseId <$> finishSavingEvt
                            
                        newRoofsDatEvt = keepLatest $ performEvent $ loadRoofAndPanels cfg <$> newHouseIdEvt

                        res = def # _hasHouse    .~ (hasHouse <$> hdEvt)
                                  # _tracerMode  .~ (traceRes ^. _tracerMode)
                                  # _saveStepEvt .~ stepEvt
                                  # _activeItem  .~ (actRoofEvt <|>
                                                     actTreeEvt <|>
                                                     const Nothing <$> delHouseEvt <|>
                                                     const Nothing <$> deactEvt)

                    pure { input: newRoofsDatEvt, output: { input: newActIdEvt, output : { input: newHdEvt, output : res } } }


runAPIEvent :: forall a. APIConfig -> Event (API (Event a)) -> Event a
runAPIEvent apiCfg = keepLatest <<< performEvent <<< map (flip runAPI apiCfg)

saveMeshes :: HouseBuilderConfig -> Event File -> Event MeshFiles -> Event JSHouses -> Event SavingStep
saveMeshes cfg imgEvt mFilesEvt houseEvt =
    let leadId = cfg ^. _leadId
        apiCfg = cfg ^. _apiConfig
        
        doUpload fs img = uploadMeshFiles leadId fs img
        uploadedEvt = multicast $ runAPIEvent apiCfg $ doUpload <$> mFilesEvt <*> imgEvt

        toCreateEvt = multicast $ sampleOn_ houseEvt uploadedEvt
        createdEvt  = multicast $ runAPIEvent apiCfg $ createManual leadId <$> toCreateEvt

        readyEvt = runAPIEvent apiCfg $ const (repeatCheckUntilReady leadId) <$> createdEvt

    in (const UploadingFiles  <$> mFilesEvt)   <|>
       (const CreatingHouse   <$> toCreateEvt) <|>
       (const WaitingForReady <$> createdEvt)  <|>
       (Finished <<< view _houseId  <$> readyEvt)


-- load roof plates and panels data after saving mesh is finished
loadRoofAndPanels :: HouseBuilderConfig -> Int -> Effect (Event RoofsData)
loadRoofAndPanels conf houseId = do
    let apiCfg = conf ^. _apiConfig
    roofsEvt  <- runAPI (loadRoofplates houseId) apiCfg
    panelsEvt <- runAPI (loadPanels houseId) apiCfg

    let mkRoofsDat rs ps = def # _houseId .~ houseId
                               # _roofs   .~ rs
                               # _panels  .~ ps

    pure $ multicast $ mkRoofsDat <$> roofsEvt <*> panelsEvt

-- | external API to build a 3D house for 2D lead
buildHouse :: Editor -> HouseBuilderConfig -> Effect HouseBuilt
buildHouse editor cfg = do
    setMode editor EditorMode.HouseBuilding
    
    { event: expEvt, push: toExp } <- create
    { event: undoTracingEvt, push: toUndoTracing } <- create
    { event: stopTracingEvt, push: toStopTracing } <- create
    { event: shadeEvt, push: selectShade } <- create
    { event: delHouseEvt, push: delHouse } <- create
    { event: treeEvt, push: buildTree } <- create

    let inputEvts = def # _export        .~ expEvt
                        # _undoTracing   .~ undoTracingEvt
                        # _stopTracing   .~ stopTracingEvt
                        # _shadeSelected .~ shadeEvt
                        # _deleteHouse   .~ delHouseEvt
                        # _buildTree     .~ treeEvt

    res <- fst <$> runNode (createHouseBuilder inputEvts) (mkNodeEnv editor cfg)
    let parentEl = unsafeCoerce $ editor ^. _parent
        conf     = def # _sizeDyn       .~ (editor ^. _sizeDyn)
                       # _showSaveDyn   .~ step false (res ^. _hasHouse <|> const false <$> expEvt)
                       # _showResetDyn  .~ step false ((==) Tracing <$> (res ^. _tracerMode))
                       # _savingStepDyn .~ step NotSaving (res ^. _saveStepEvt)
                       # _activeItemDyn .~ step Nothing (res ^. _activeItem)
                   
    uiEvts <- runMainWidgetInNode parentEl $ houseBuilderUI conf

    void $ subscribe (const unit <$> uiEvts ^. _buttons <<< _save) toExp
    void $ subscribe (const unit <$> uiEvts ^. _buttons <<< _reset) toStopTracing
    void $ subscribe (const unit <$> uiEvts ^. _buttons <<< _undo) toUndoTracing
    void $ subscribe (uiEvts ^. _shadeSelected) selectShade
    void $ subscribe (uiEvts ^. _deleteHouse) delHouse
    void $ subscribe (uiEvts ^. _buildTree) buildTree

    pure $ res # _editorOp .~ (const Close <$> uiEvts ^. _buttons <<< _close)
