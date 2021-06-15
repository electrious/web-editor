module SmartHouse.HouseBuilder (buildHouse, HouseBuilderConfig, HouseBuilt,  _hasHouse) where

import Prelude hiding (degree)

import API (API, APIConfig, runAPI)
import API.Image (ImageResp, _link, _pixelPerMeter, getImageMeta)
import API.Panel (loadPanels)
import API.Racking (loadRacking)
import API.Roofplate (loadRoofplates)
import API.SmartHouse (SavingStep(..), createManual, isFinished, repeatCheckUntilReady, savedHouseId, uploadMeshFiles)
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Monad.Except (runExcept)
import Custom.Mesh (TapMouseMesh)
import Data.Array (fromFoldable)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as M
import Data.Maybe (Maybe(..), isNothing)
import Data.Meter (meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.UUID (UUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_apiConfig, _deleted, _height, _houseId, _leadId, _modeDyn, _mouseMove, _name, _panelType, _panels, _parent, _roofRackings, _roofs, _tapped, _textureInfo, _updated, _width)
import Editor.Editor (Editor, _sizeDyn, setMode)
import Editor.EditorMode as EditorMode
import Editor.HouseEditor (ArrayEditParam, HouseConfig, _dataServer, _heatmapTexture, _rotBtnTexture)
import Editor.RoofManager (RoofsData, _racks)
import Editor.SceneEvent (size)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, step)
import FRP.Event (Event, create, keepLatest, sampleOn, sampleOn_, subscribe)
import FRP.Event.Extra (delay, multicast, performEvent)
import Math.Angle (degree)
import Model.ActiveMode (ActiveMode(..), fromBoolean)
import Model.Hardware.PanelTextureInfo (PanelTextureInfo)
import Model.Hardware.PanelType (PanelType(..))
import Model.SmartHouse.House (House, HouseNode, HouseOp(..), JSHouses(..), createHouseFrom, exportHouse, houseTapped)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _imageFile, _size, _texture, mkHouseTextureInfo)
import Model.UUID (idLens)
import OBJExporter (MeshFiles, exportObject)
import Rendering.DynamicNode (eventNode)
import Rendering.Node (Node, fixNodeDWith, fixNodeE, fixNodeEWith, getEnv, getParent, localEnv, mkNodeEnv, node, runNode, tapMouseMesh)
import Rendering.TextureLoader (textureFromUrl)
import SmartHouse.BuilderMode (BuilderMode(..))
import SmartHouse.HouseEditor (HouseRenderMode(..), _arrayEditParam, _house, _roofsData, editHouse, renderHouse)
import SmartHouse.HouseTracer (TracerMode(..), _stopTracing, _tracedPolygon, _tracerMode, traceHouse)
import SmartHouse.UI (_savingStepDyn, houseBuilderUI)
import Specular.Dom.Widget (runMainWidgetInNode)
import Three.Core.Geometry (mkPlaneGeometry)
import Three.Core.Material (mkMeshBasicMaterialWithTexture)
import Three.Loader.TextureLoader (clampToEdgeWrapping, repeatWrapping, setRepeat, setWrapS, setWrapT, textureHeight, textureImageEvt, textureWidth)
import UI.ButtonPane (_close, _reset, _save, _showResetDyn, _showSaveDyn)
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

derive instance newtypeHouseBuilderConfig :: Newtype HouseBuilderConfig _
instance defaultHouseBuilderConfig :: Default HouseBuilderConfig where
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
    editorOp    :: Event EditorUIOp
    }

derive instance newtypeHouseBuilt :: Newtype HouseBuilt _
instance defaultHouseBuilt :: Default HouseBuilt where
    def = HouseBuilt {
        hasHouse    : empty,
        tracerMode  : empty,
        saveStepEvt : empty,
        editorOp    : empty
        }

_hasHouse :: forall t a r. Newtype t { hasHouse :: a | r } => Lens' t a
_hasHouse = _Newtype <<< prop (SProxy :: SProxy "hasHouse")

_saveStepEvt :: forall t a r. Newtype t { saveStepEvt :: a | r } => Lens' t a
_saveStepEvt = _Newtype <<< prop (SProxy :: SProxy "saveStepEvt")

compactHouseBuilt :: Event HouseBuilt -> HouseBuilt
compactHouseBuilt e = def # _hasHouse    .~ keepLatest (view _hasHouse    <$> e)
                          # _tracerMode  .~ keepLatest (view _tracerMode  <$> e)
                          # _saveStepEvt .~ keepLatest (view _saveStepEvt <$> e)
                          # _editorOp    .~ keepLatest (view _editorOp    <$> e)


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

    tapMouseMesh (def # _name .~ "helper-plane") geo mat

-- internal data structure to manage houses
type HouseDict = UUIDMap House

newtype HouseDictData = HouseDictData {
    houses         :: HouseDict,
    housesToRender :: Maybe HouseDict
    }

derive instance newtypeHouseDictData :: Newtype HouseDictData _
derive instance genericHouseDictData :: Generic HouseDictData _
instance showHouseDictData :: Show HouseDictData where
    show = genericShow
instance defaultHouseDictData :: Default HouseDictData where
    def = HouseDictData {
        houses         : M.empty,
        housesToRender : Nothing
        }

_houses :: forall t a r. Newtype t { houses :: a | r } => Lens' t a
_houses = _Newtype <<< prop (SProxy :: SProxy "houses")

_housesToRender :: forall t a r. Newtype t { housesToRender :: a | r } => Lens' t a
_housesToRender = _Newtype <<< prop (SProxy :: SProxy "housesToRender")

hasHouse :: HouseDictData -> Boolean
hasHouse = not <<< M.isEmpty <<< view _houses

-- mark all houses to be rendered
renderAll :: HouseDictData -> HouseDictData
renderAll s = s # _housesToRender .~ Just (s ^. _houses)

-- | update the HouseDictData with house operations
applyHouseOp :: HouseOp -> HouseDictData -> HouseDictData
applyHouseOp (HouseOpCreate house) d = renderAll $ d # _houses %~ M.insert (house ^. idLens) house
applyHouseOp (HouseOpDelete hid)   d = renderAll $ d # _houses %~ M.delete hid
applyHouseOp (HouseOpUpdate house) d = d # _houses %~ M.insert (house ^. idLens) house
                                         # _housesToRender .~ Nothing


exportHouses :: HouseDictData -> JSHouses
exportHouses hd = JSHouses { houses : fromFoldable $ M.values $ exportHouse <$> hd ^. _houses }

-- get house update event from a list of house nodes
getHouseUpd :: forall f. Foldable f => Functor f => f HouseNode -> Event HouseOp
getHouseUpd = foldEvtWith (view _updated)

-- get house delete event from a list of house nodes
getHouseDel :: forall f. Foldable f => Functor f => f HouseNode -> Event HouseOp
getHouseDel = foldEvtWith (view _deleted)

-- get the activated house from a list of house nodes
getActivated :: forall f. Foldable f => Functor f => f HouseNode -> Event UUID
getActivated = foldEvtWith f
    where f n = const (n ^. idLens) <$> houseTapped n

renderHouseDict :: Dynamic (Maybe UUID) -> Dynamic BuilderMode -> HouseConfig -> ArrayEditParam -> Event RoofsData -> HouseDict -> Node HouseTextureInfo (UUIDMap HouseNode)
renderHouseDict actHouseDyn modeDyn houseCfg arrParam roofsDatEvt houses = traverse render houses
    where getMode _ _ Showing                            = Inactive
          getMode h (Just i) Building | h ^. idLens == i = Active
                                      | otherwise        = Inactive
          getMode h Nothing Building                     = Inactive
          
          render h = if houseRenderMode == EditHouseMode
                     then editHouse houseCfg $ def # _modeDyn        .~ (getMode h <$> actHouseDyn <*> modeDyn)
                                                   # _house          .~ h
                                                   # _roofsData      .~ roofsDatEvt
                                                   # _arrayEditParam .~ arrParam
                     else renderHouse h


tracerMode :: Maybe UUID -> BuilderMode -> ActiveMode
tracerMode _ Showing  = Inactive
tracerMode h Building = fromBoolean $ isNothing h

newtype BuilderInputEvts = BuilderInputEvts {
    export      :: Event Unit,
    stopTracing :: Event Unit
    }

derive instance newtypeBuilderInputEvts :: Newtype BuilderInputEvts _
instance defaultBuilderInputEvts :: Default BuilderInputEvts where
    def = BuilderInputEvts {
        export      : empty,
        stopTracing : empty
        }

_export :: forall t a r. Newtype t { export :: a | r } => Lens' t a
_export = _Newtype <<< prop (SProxy :: SProxy "export")


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
                                 # _modeDyn        .~ pure EditorMode.RoofEditing
                                 # _textureInfo    .~ (cfg ^. _textureInfo)
                                 # _rotBtnTexture  .~ (cfg ^. _rotBtnTexture)
                                 # _heatmapTexture .~ (cfg ^. _heatmapTexture)
                                 # _panelType      .~ (cfg ^. _panelType)
                                 # _apiConfig      .~ (cfg ^. _apiConfig)

builderForHouse :: BuilderInputEvts -> HouseTextureInfo -> Node HouseBuilderConfig HouseBuilt
builderForHouse evts tInfo =
    fixNodeEWith def \hdEvt ->
        fixNodeDWith Nothing \actHouseDyn ->
            fixNodeDWith Building \modeDyn ->
                fixNodeE \roofsDatEvt -> do
                    pNode <- getParent
                    cfg   <- getEnv
                    
                    -- add helper plane that accepts tap and drag events
                    helper <- mkHelperPlane tInfo

                    -- render all houses
                    let houseToRenderEvt = compact $ view _housesToRender <$> hdEvt
                        houseCfg = houseCfgFromBuilderCfg cfg

                        arrParam = def
                        
                    nodesEvt <- localEnv (const tInfo) $ eventNode (renderHouseDict actHouseDyn modeDyn houseCfg arrParam roofsDatEvt <$> houseToRenderEvt)

                    let deactEvt    = multicast $ const Nothing <$> helper ^. _tapped
                        actEvt      = keepLatest $ getActivated <$> nodesEvt
                        actHouseEvt = (Just <$> actEvt) <|> deactEvt

                    -- trace new house
                    traceRes <- traceHouse $ def # _modeDyn     .~ (tracerMode <$> actHouseDyn <*> modeDyn)
                                                 # _mouseMove   .~ helper ^. _mouseMove
                                                 # _stopTracing .~ (evts ^. _stopTracing)
                    let houseEvt = performEvent $ createHouseFrom (degree 30.0) <$> (traceRes ^. _tracedPolygon)
                        addHouseEvt = HouseOpCreate <$> houseEvt
                        updHouseEvt = keepLatest (getHouseUpd <$> nodesEvt)

                        opEvt = addHouseEvt <|> updHouseEvt

                        -- update HouseDictData by applying the house operations
                        newHdEvt = multicast $ sampleOn hdEvt $ applyHouseOp <$> opEvt

                        exportEvt = multicast $ evts ^. _export
                    
                        toExpEvt = delay 15 exportEvt
                    
                        meshFilesEvt = performEvent $ const (exportObject pNode) <$> toExpEvt
                        housesEvt    = exportHouses <$> sampleOn_ hdEvt toExpEvt

                        stepEvt = multicast $ saveMeshes cfg (tInfo ^. _imageFile) meshFilesEvt housesEvt

                        -- get the new generated house id after saving finished
                        finishSavingEvt = multicast $ filter isFinished stepEvt
                        newHouseIdEvt = compact $ savedHouseId <$> finishSavingEvt
                            
                        newRoofsDatEvt = keepLatest $ performEvent $ loadRoofAndPanels cfg <$> newHouseIdEvt
                    
                        modeEvt = (const Showing <$> exportEvt) <|> 
                                  (const Building <$> finishSavingEvt)
                    
                        res = def # _hasHouse    .~ (hasHouse <$> hdEvt)
                                  # _tracerMode  .~ (traceRes ^. _tracerMode)
                                  # _saveStepEvt .~ stepEvt

                    pure { input: newRoofsDatEvt, output : { input: modeEvt, output: { input: actHouseEvt, output : { input: newHdEvt, output : res } } } }


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
    racksEvt  <- runAPI (loadRacking houseId) apiCfg

    let mkRoofsDat rs ps racks = def # _houseId .~ houseId
                                     # _roofs   .~ rs
                                     # _panels  .~ ps
                                     # _racks   .~ extrRoofRack racks

        extrRoofRack res = case runExcept res of
            Left _ -> M.empty
            Right v -> v ^. _roofRackings

    pure $ mkRoofsDat <$> roofsEvt <*> panelsEvt <*> racksEvt

-- | external API to build a 3D house for 2D lead
buildHouse :: Editor -> HouseBuilderConfig -> Effect HouseBuilt
buildHouse editor cfg = do
    setMode editor EditorMode.HouseBuilding
    
    { event: expEvt, push: toExp } <- create
    { event: stopTracingEvt, push: toStopTracing } <- create

    let inputEvts = def # _export      .~ expEvt
                        # _stopTracing .~ stopTracingEvt

    res <- fst <$> runNode (createHouseBuilder inputEvts) (mkNodeEnv editor cfg)
    let parentEl = unsafeCoerce $ editor ^. _parent
        conf     = def # _sizeDyn       .~ (editor ^. _sizeDyn)
                       # _showSaveDyn   .~ step false (res ^. _hasHouse)
                       # _showResetDyn  .~ step false ((==) Tracing <$> (res ^. _tracerMode))
                       # _savingStepDyn .~ step NotSaving (res ^. _saveStepEvt)
    uiEvts <- runMainWidgetInNode parentEl $ houseBuilderUI conf

    void $ subscribe (const unit <$> uiEvts ^. _save) toExp
    void $ subscribe (const unit <$> uiEvts ^. _reset) toStopTracing

    pure $ res # _editorOp .~ (const Close <$> uiEvts ^. _close)
