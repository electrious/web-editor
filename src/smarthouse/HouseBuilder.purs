module SmartHouse.HouseBuilder (buildHouse, HouseBuilderConfig(..), HouseBuilt(..),  _hasHouse) where

import Prelude hiding (degree)

import API (API, APIConfig, APIError(..), _xCompanyId, runAPI, showAPIError)
import API.Image (ImageResp, _link, _pixelPerMeter, getImageMeta)
import API.Panel (loadPanels)
import API.Roofplate (loadRoofplates)
import API.SmartHouse (BuiltHouseInfo, ReadyAPIResp, SavingStep(..), _companyId, createManual, isFinished, mkBuiltHouseInfo, repeatCheckUntilReady, savedHouseInfo, uploadMeshFiles)
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Custom.Mesh (TapMouseMesh)
import Data.Array (fromFoldable)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Data.Filterable (filter)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', set, view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as M
import Data.Maybe (Maybe(..), isNothing)
import Data.Meter (meterVal)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Data.UUIDMap (UUIDMap)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_apiConfig, _buildChimney, _buildTree, _buttons, _deleted, _height, _houseId, _leadId, _modeDyn, _mouseMove, _name, _panelType, _panels, _parent, _roofs, _slopeSelected, _tapped, _textureInfo, _updated, _width)
import Editor.Editor (Editor, _sizeDyn, setMode)
import Editor.EditorMode as EditorMode
import Editor.HouseEditor (ArrayEditParam, HouseConfig, _dataServer, _heatmapTexture, _rotBtnTexture)
import Editor.RoofManager (RoofsData)
import Editor.SceneEvent (SceneTapEvent, size)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, current, dynEvent, performDynamic, sampleDyn, step)
import FRP.Event (Event, create, keepLatest, sampleOn, sampleOn_, subscribe)
import FRP.Event.Extra (delay, multicast, performEvent)
import Model.ActiveMode (ActiveMode(..), fromBoolean)
import Model.Hardware.PanelTextureInfo (PanelTextureInfo)
import Model.Hardware.PanelType (PanelType(..))
import Model.Polygon (Polygon)
import Model.SmartHouse.House (House, JSHouses(..), _trees, createHouseFrom, defaultSlope, exportHouse)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _imageFile, _size, _texture, mkHouseTextureInfo)
import Model.SmartHouse.Tree (Tree, TreeNode, TreeOp(..))
import Model.UUID (class HasUUID, idLens)
import Models.SmartHouse.ActiveItem (ActHouseRoof, ActiveItem(..))
import OBJExporter (MeshFiles, exportObject)
import Rendering.DynamicNode (eventNode)
import Rendering.Node (Node, _exportable, fixNodeDWith, fixNodeE, fixNodeE2With, getEnv, getParent, localEnv, mkNodeEnv, node, runNode, tapMouseMesh)
import Rendering.TextureLoader (textureFromUrl)
import SmartHouse.HouseEditor (HouseRenderMode(..), _arrayEditParam, _house, _roofsData, editHouse, renderHouse)
import SmartHouse.HouseTracer (TracerMode(..), _stopTracing, _tracedPolygon, _tracerMode, _undoTracing, traceHouse)
import SmartHouse.SlopeOption (SlopeOption)
import SmartHouse.TreeBuilder (buildTree, editTree)
import SmartHouse.UI (_activeItemDyn, _savingStepDyn, houseBuilderUI)
import Smarthouse.HouseNode (HouseNode, HouseOp(..), _actHouseRoof, _activated)
import Specular.Dom.Widget (runMainWidgetInNode)
import Three.Core.Geometry (mkPlaneGeometry)
import Three.Core.Material (mkMeshBasicMaterialWithTexture)
import Three.Loader.TextureLoader (clampToEdgeWrapping, repeatWrapping, setRepeat, setWrapS, setWrapT, textureHeight, textureImageEvt, textureWidth)
import Three.Math.Vector (Vector3)
import Type.Proxy (Proxy(..))
import UI.ButtonPane (_close, _reset, _save, _showResetDyn, _showSaveDyn, _undo)
import UI.EditorUIOp (EditorUIOp(..))
import UI.RoofEditorUI (_editorOp)
import Unsafe.Coerce (unsafeCoerce)
import Util (foldEvtWith)
import Web.File.File (File)

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
    apiConfig      :: Dynamic APIConfig
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
        apiConfig      : pure def
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

getActiveRoof :: forall f. Foldable f => Functor f => f HouseNode -> Event ActHouseRoof
getActiveRoof = foldEvtWith (view _actHouseRoof)

-- get the activated house from a list of house nodes
getActivated :: forall f. Foldable f => Functor f => f HouseNode -> Event UUID
getActivated = foldEvtWith f
    where f n = const (n ^. idLens) <$> n ^. _activated

-- | get tapped event from a foldable list of values that have tapped field
getTapped :: forall f n r a. HasUUID n => Newtype n { tapped :: Event a | r } => Foldable f => Functor f => f n -> Event UUID
getTapped = foldEvtWith f
    where f n = const (n ^. idLens) <$> n ^. _tapped

getActive :: forall v. (HouseDictData -> UUIDMap v) -> UUID -> HouseDictData -> Maybe v
getActive f tid = M.lookup tid <<< f

getUpdEvt :: forall f n o r. Foldable f => Functor f => Newtype n { updated :: Event o | r } => f n -> Event o
getUpdEvt = foldEvtWith (view _updated)

updateHouseConfig :: HouseConfig -> Event (Maybe Int) -> Effect HouseConfig
updateHouseConfig cfg compIdEvt = do
    let apiCfgDyn = cfg ^. _apiConfig

    defCfg <- current apiCfgDyn

    let cfgEvt = dynEvent apiCfgDyn <|>
                 sampleDyn apiCfgDyn (set _xCompanyId <$> compIdEvt)

    pure $ cfg # _apiConfig .~ step defCfg cfgEvt


renderHouseDict :: Dynamic (Maybe UUID) -> HouseConfig -> ArrayEditParam -> Event SlopeOption -> Event RoofsData -> HouseDict -> Node HouseTextureInfo (UUIDMap HouseNode)
renderHouseDict actIdDyn houseCfg arrParam slopeEvt roofsDatEvt houses = traverse render houses
    where render h = if houseRenderMode == EditHouseMode
                     then do
                         let md = getMode h <$> actIdDyn
                         editHouse houseCfg $ def # _modeDyn        .~ md
                                                  # _house          .~ h
                                                  # _slopeSelected  .~ slopeEvt
                                                  # _roofsData      .~ roofsDatEvt
                                                  # _arrayEditParam .~ arrParam
                     else renderHouse h

renderTrees :: forall e. Dynamic (Maybe UUID) -> TreeDict -> Node e (UUIDMap TreeNode)
renderTrees actIdDyn trees = traverse render trees
    where render t = editTree t (getMode t <$> actIdDyn)

getMode :: forall a. HasUUID a => a -> Maybe UUID -> ActiveMode
getMode c (Just i) | c ^. idLens == i = Active
                   | otherwise        = Inactive
getMode _ Nothing                     = Inactive


tracerMode :: Maybe UUID -> Boolean -> Boolean -> ActiveMode
tracerMode h t c = fromBoolean $ isNothing h && not t && not c

builderMode :: Maybe UUID -> Boolean -> ActiveMode
builderMode h t = fromBoolean $ isNothing h && t

newtype BuilderInputEvts = BuilderInputEvts {
    export        :: Event Unit,
    undoTracing   :: Event Unit,
    stopTracing   :: Event Unit,
    slopeSelected :: Event SlopeOption,
    deleted       :: Event Unit,
    buildTree     :: Event Boolean,
    buildChimney  :: Event Boolean
    }

derive instance Newtype BuilderInputEvts _
instance Default BuilderInputEvts where
    def = BuilderInputEvts {
        export        : empty,
        undoTracing   : empty,
        stopTracing   : empty,
        slopeSelected : empty,
        deleted       : empty,
        buildTree     : empty,
        buildChimney  : empty
        }

_export :: forall t a r. Newtype t { export :: a | r } => Lens' t a
_export = _Newtype <<< prop (Proxy :: Proxy "export")


createHouseBuilder :: BuilderInputEvts -> Node HouseBuilderConfig HouseBuilt
createHouseBuilder evts = node (def # _name .~ "house-builder") $ do
    cfg <- getEnv
    let lId = cfg ^. _leadId

    apiCfg <- liftEffect $ current $ cfg ^. _apiConfig
    -- load image meta info
    imgEvt <- liftEffect $ runAPI (getImageMeta $ def # _leadId .~ lId) apiCfg

    -- load texture image
    let tInfoEvt = keepLatest $ loadHouseTexture <$> imgEvt
    evt <- eventNode $ builderForHouse evts <$> tInfoEvt
    pure $ compactHouseBuilt evt


-- create house from the traced polygon
getHouseOpEvt :: Event (UUIDMap HouseNode) -> Dynamic (Maybe UUID) -> Event (Polygon Vector3) -> Event Unit -> Event HouseOp
getHouseOpEvt houseNodesEvt actIdDyn polyEvt delEvt =
    let polyDyn   = step Nothing $ Just <$> polyEvt
        houseDyn  = performDynamic $ traverse (createHouseFrom defaultSlope) <$> polyDyn
        houseEvt  = compact $ dynEvent houseDyn

        addHouseEvt = HouseOpCreate <$> houseEvt
        updHouseEvt = keepLatest $ getUpdEvt <$> houseNodesEvt
        delHouseEvt = multicast $ compact $ sampleDyn actIdDyn $ (const (map HouseOpDelete)) <$> delEvt
    in addHouseEvt <|> updHouseEvt <|> delHouseEvt

-- tree operations
getTreeOpEvt :: Event (UUIDMap TreeNode) -> Dynamic (Maybe UUID) -> Event Unit -> Event Tree -> Event TreeOp
getTreeOpEvt treeNodesEvt actIdDyn delEvt newTreeEvt = 
    let addTreeEvt = TreeOpCreate <$> newTreeEvt
        delTreeEvt = multicast $ compact $ sampleDyn actIdDyn $ (const (map TreeOpDelete)) <$> delEvt
        updTreeEvt = keepLatest $ getUpdEvt <$> treeNodesEvt

    in addTreeEvt <|> updTreeEvt <|> delTreeEvt

-- manage active item
getActiveItemEvt :: Event (UUIDMap HouseNode) -> Event (UUIDMap TreeNode) -> Event SceneTapEvent -> Event HouseDictData -> Event Unit -> Tuple (Event (Maybe UUID)) (Event (Maybe ActiveItem))
getActiveItemEvt houseNodesEvt treeNodesEvt helperTapEvt hdEvt delEvt =
    let deactEvt   = multicast $ const Nothing <$> helperTapEvt

        actHouseEvt = keepLatest $ getActivated <$> houseNodesEvt
        actRoofEvt  = Just <$> keepLatest (map ActiveHouse <<< getActiveRoof <$> houseNodesEvt)

        treeTapEvt = multicast $ keepLatest $ getTapped <$> treeNodesEvt 
        actTreeEvt = map ActiveTree <$> (sampleOn hdEvt $ getActive (view _trees) <$> treeTapEvt)

        newActIdEvt = (Just <$> (actHouseEvt <|> treeTapEvt)) <|> deactEvt

        actItemEvt = actRoofEvt <|> actTreeEvt <|> const Nothing <$> delEvt <|> const Nothing <$> deactEvt
    in Tuple newActIdEvt actItemEvt


-- export scene and mesh files
exportScene :: HouseTextureInfo -> Event HouseDictData -> Event Unit -> Node HouseBuilderConfig (Tuple (Event SavingStep) (Event BuiltHouseInfo))
exportScene tInfo hdEvt toExpEvt = do
    pNode <- getParent
    cfg   <- getEnv

    let meshFilesEvt = performEvent $ const (exportObject pNode) <$> toExpEvt
        housesEvt    = exportHouses <$> sampleOn_ hdEvt toExpEvt

        stepEvt = multicast $ saveMeshes cfg (tInfo ^. _imageFile) meshFilesEvt housesEvt

        -- get the new generated house id after saving finished
        houseIdEvt = multicast $ compact $ savedHouseInfo <$> filter isFinished stepEvt
    pure $ Tuple stepEvt houseIdEvt

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
    fixNodeE2With def Nothing \hdEvt companyIdEvt ->
        fixNodeDWith Nothing \actIdDyn ->
            fixNodeE \roofsDatEvt -> do
                cfg <- getEnv

                -- add helper plane that accepts tap and drag events
                helper <- mkHelperPlane tInfo

                -- render all houses
                let houseToRenderEvt = compact $ view _housesToRender <$> hdEvt
                    treesToRenderEvt = compact $ view _treesToRender <$> hdEvt

                    slopeEvt = evts ^. _slopeSelected

                    mouseEvt = multicast $ helper ^. _mouseMove
                    delEvt   = multicast $ evts ^. _deleted
                    toExpEvt = multicast $ delay 15 $ evts ^. _export

                houseCfg <- liftEffect $ updateHouseConfig (houseCfgFromBuilderCfg cfg) companyIdEvt

                -- render houses and trees
                houseNodesEvt <- localEnv (const tInfo) $ eventNode (renderHouseDict actIdDyn houseCfg def slopeEvt roofsDatEvt <$> houseToRenderEvt)
                treeNodesEvt  <- eventNode (renderTrees actIdDyn <$> treesToRenderEvt)

                let Tuple newActIdEvt actItemEvt = getActiveItemEvt houseNodesEvt treeNodesEvt (helper ^. _tapped) hdEvt delEvt
                    buildTreeDyn = step false $ evts ^. _buildTree
                    buildChimDyn = step false $ evts ^. _buildChimney

                -- trace new house
                traceRes <- traceHouse $ def # _modeDyn     .~ (tracerMode <$> actIdDyn <*> buildTreeDyn <*> buildChimDyn)
                                             # _mouseMove   .~ mouseEvt
                                             # _undoTracing .~ (evts ^. _undoTracing)
                                             # _stopTracing .~ (evts ^. _stopTracing)

                newTreeEvt <- buildTree (builderMode <$> actIdDyn <*> buildTreeDyn) mouseEvt

                let houseOpEvt = getHouseOpEvt houseNodesEvt actIdDyn (traceRes ^. _tracedPolygon) delEvt
                    treeOpEvt  = getTreeOpEvt treeNodesEvt actIdDyn delEvt newTreeEvt

                    -- update HouseDictData by applying the house operations
                    newHdEvt = multicast $ sampleOn hdEvt $ (applyHouseOp <$> houseOpEvt) <|>
                                                            (applyTreeOp <$> treeOpEvt)

                -- export scene 
                Tuple stepEvt builtHouseInfoEvt <- exportScene tInfo newHdEvt toExpEvt
                let compIdEvt = Just <<< view _companyId <$> builtHouseInfoEvt

                    -- load roofplate and panel data
                    newRoofsDatEvt = keepLatest $ performEvent $ loadRoofAndPanels cfg <$> builtHouseInfoEvt

                    res = def # _hasHouse    .~ (hasHouse <$> hdEvt)
                              # _tracerMode  .~ (traceRes ^. _tracerMode)
                              # _saveStepEvt .~ stepEvt
                              # _activeItem  .~ actItemEvt

                pure { input: newRoofsDatEvt, output: { input: newActIdEvt, output : { input1: newHdEvt, input2: compIdEvt, output : res } } }


runAPIEvent :: forall a. Dynamic APIConfig -> Event (API (Event a)) -> Event a
runAPIEvent apiCfg = keepLatest <<< performEvent <<< sampleDyn apiCfg <<< map runAPI

defError :: APIError
defError = StubError

saveMeshes :: HouseBuilderConfig -> Event File -> Event MeshFiles -> Event JSHouses -> Event SavingStep
saveMeshes cfg imgEvt mFilesEvt houseEvt =
    let leadId = cfg ^. _leadId
        apiCfg = cfg ^. _apiConfig
        
        doUpload fs img = uploadMeshFiles leadId fs img
        uploadedEvt = multicast $ runAPIEvent apiCfg $ doUpload <$> mFilesEvt <*> imgEvt
        
        uploadFailedEvt = fromLeft defError <$> filter isLeft uploadedEvt
        uploadSuccEvt = filter isRight uploadedEvt

        toCreateEvt = multicast $ sampleOn_ houseEvt uploadSuccEvt
        createdEvt  = multicast $ runAPIEvent apiCfg $ createManual leadId <$> toCreateEvt

        createFailedEvt = fromLeft defError <$> filter isLeft createdEvt
        createSuccEvt = filter isRight createdEvt

        readyEvt = runAPIEvent apiCfg $ const (repeatCheckUntilReady leadId) <$> createSuccEvt

        readyFailedEvt = fromLeft defError <$> filter isLeft readyEvt
        readySuccEvt = fromRight def <$> filter isRight readyEvt

        failedEvt = delay 300 $ (UploadFailed <<< showAPIError <$> uploadFailedEvt) <|>
                                (CreatingFailed <<< showAPIError <$> createFailedEvt) <|>
                                (ReadyFailed <<< showAPIError <$> readyFailedEvt)

    in (const UploadingFiles  <$> mFilesEvt)   <|>
       (const CreatingHouse   <$> toCreateEvt) <|>
       (const WaitingForReady <$> createSuccEvt)  <|>
       failedEvt <|>
       (mkFinished <$> readySuccEvt)

mkFinished :: ReadyAPIResp -> SavingStep
mkFinished r = Finished $ mkBuiltHouseInfo (r ^. _houseId) (r ^. _companyId)

-- load roof plates and panels data after saving mesh is finished
loadRoofAndPanels :: HouseBuilderConfig -> BuiltHouseInfo -> Effect (Event RoofsData)
loadRoofAndPanels conf info = do
    cfg <- current $ conf ^. _apiConfig
    let compId = info ^. _companyId
        houseId = info ^. _houseId
        apiCfg = cfg # _xCompanyId .~ Just compId
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
    { event: slopeEvt, push: selectSlope } <- create
    { event: delHouseEvt, push: delHouse } <- create
    { event: treeEvt, push: buildTree } <- create
    { event: chimEvt, push: buildChim } <- create

    let inputEvts = def # _export        .~ expEvt
                        # _undoTracing   .~ undoTracingEvt
                        # _stopTracing   .~ stopTracingEvt
                        # _slopeSelected .~ slopeEvt
                        # _deleted       .~ delHouseEvt
                        # _buildTree     .~ treeEvt
                        # _buildChimney  .~ chimEvt

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
    void $ subscribe (uiEvts ^. _slopeSelected) selectSlope
    void $ subscribe (uiEvts ^. _deleted) delHouse
    void $ subscribe (uiEvts ^. _buildTree) buildTree
    void $ subscribe (uiEvts ^. _buildChimney) buildChim

    pure $ res # _editorOp .~ (const Close <$> uiEvts ^. _buttons <<< _close)
