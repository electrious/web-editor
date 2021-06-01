module SmartHouse.HouseBuilder (buildHouse, HouseBuilderConfig, HouseBuilt,  _hasHouse) where

import Prelude hiding (degree)

import API (API, APIConfig, runAPI)
import API.Image (ImageResp, _link, _pixelPerMeter, getImageMeta)
import API.SmartHouse (SavingStep(..), createManual, repeatCheckUntilReady, uploadMeshFiles)
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Custom.Mesh (TapMouseMesh)
import Data.Array (fromFoldable)
import Data.Compactable (compact)
import Data.Default (class Default, def)
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
import Editor.Common.Lenses (_apiConfig, _deleted, _height, _leadId, _modeDyn, _mouseMove, _name, _parent, _tapped, _updated, _width)
import Editor.Editor (Editor, _sizeDyn, setMode)
import Editor.EditorMode as EditorMode
import Editor.SceneEvent (size)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, step)
import FRP.Event (Event, create, keepLatest, sampleOn, sampleOn_, subscribe)
import FRP.Event.Extra (delay, multicast, performEvent)
import Math.Angle (degree)
import Model.ActiveMode (ActiveMode(..), fromBoolean)
import Model.SmartHouse.House (House, HouseNode, HouseOp(..), JSHouses(..), createHouseFrom, exportHouse, houseTapped)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _imageDataURI, _size, _texture, mkHouseTextureInfo)
import Model.UUID (idLens)
import OBJExporter (MeshFiles, exportObject)
import Rendering.DynamicNode (eventNode)
import Rendering.Node (Node, fixNodeDWith, fixNodeEWith, getEnv, getParent, localEnv, mkNodeEnv, node, runNode, tapMouseMesh)
import Rendering.TextureLoader (textureFromUrl)
import SmartHouse.BuilderMode (BuilderMode(..))
import SmartHouse.HouseEditor (HouseRenderMode(..), editHouse, renderHouse)
import SmartHouse.HouseTracer (TracerMode(..), _stopTracing, _tracedPolygon, _tracerMode, traceHouse)
import SmartHouse.UI (houseBuilderUI)
import Specular.Dom.Widget (runMainWidgetInNode)
import Three.Core.Geometry (mkPlaneGeometry)
import Three.Core.Material (mkMeshBasicMaterialWithTexture)
import Three.Loader.TextureLoader (clampToEdgeWrapping, repeatWrapping, setRepeat, setWrapS, setWrapT, textureDataURI, textureHeight, textureWidth)
import UI.ButtonPane (_close, _reset, _save, _showResetDyn, _showSaveDyn)
import UI.EditorUIOp (EditorUIOp(..))
import UI.RoofEditorUI (_editorOp)
import Unsafe.Coerce (unsafeCoerce)
import Util (foldEvtWith)

-- NOTE: global value to toggle between rendering house as full 3D editor or 2D wireframes
houseRenderMode :: HouseRenderMode
houseRenderMode = EditHouseMode


newtype HouseBuilderConfig = HouseBuilderConfig {
    leadId    :: Int,
    apiConfig :: APIConfig
}

derive instance newtypeHouseBuilderConfig :: Newtype HouseBuilderConfig _
instance defaultHouseBuilderConfig :: Default HouseBuilderConfig where
    def = HouseBuilderConfig { leadId : 0, apiConfig : def }

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
                  d = textureDataURI t
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

renderHouseDict :: Dynamic (Maybe UUID) -> Dynamic BuilderMode -> HouseDict -> Node HouseTextureInfo (UUIDMap HouseNode)
renderHouseDict actHouseDyn modeDyn houses = traverse render houses
    where getMode _ _ Showing                            = Inactive
          getMode h (Just i) Building | h ^. idLens == i = Active
                                      | otherwise        = Inactive
          getMode h Nothing Building                     = Inactive
          
          render h = if houseRenderMode == EditHouseMode
                     then editHouse (getMode h <$> actHouseDyn <*> modeDyn) h
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


builderForHouse :: BuilderInputEvts -> HouseTextureInfo -> Node HouseBuilderConfig HouseBuilt
builderForHouse evts tInfo =
    fixNodeEWith def \hdEvt ->
        fixNodeDWith Nothing \actHouseDyn ->
            fixNodeDWith Building \modeDyn -> do
                pNode <- getParent
                cfg   <- getEnv
                
                -- add helper plane that accepts tap and drag events
                helper <- mkHelperPlane tInfo

                -- render all houses
                let houseToRenderEvt = compact $ view _housesToRender <$> hdEvt
                nodesEvt <- localEnv (const tInfo) $ eventNode (renderHouseDict actHouseDyn modeDyn <$> houseToRenderEvt)

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
                    
                    modeEvt = (const Showing <$> exportEvt) <|> 
                              (const Building <$> delay 30 exportEvt)
                    toExpEvt = delay 15 exportEvt

                    meshFilesEvt = performEvent $ const (exportObject pNode) <$> toExpEvt
                    housesEvt    = exportHouses <$> sampleOn_ hdEvt toExpEvt

                    stepEvt = saveMeshes cfg (tInfo ^. _imageDataURI) meshFilesEvt housesEvt 
                    
                    res = def # _hasHouse    .~ (hasHouse <$> hdEvt)
                              # _tracerMode  .~ (traceRes ^. _tracerMode)
                              # _saveStepEvt .~ stepEvt

                pure { input: modeEvt, output: { input: actHouseEvt, output : { input: newHdEvt, output : res } } }


runAPIEvent :: forall a. APIConfig -> Event (API (Event a)) -> Event a
runAPIEvent apiCfg = keepLatest <<< performEvent <<< map (flip runAPI apiCfg)

saveMeshes :: HouseBuilderConfig -> String -> Event MeshFiles -> Event JSHouses -> Event SavingStep
saveMeshes cfg imgStr mFilesEvt houseEvt =
    let leadId = cfg ^. _leadId
        apiCfg = cfg ^. _apiConfig
        
        doUpload fs = uploadMeshFiles leadId fs imgStr
        uploadedEvt = multicast $ runAPIEvent apiCfg $ doUpload <$> mFilesEvt

        toCreateEvt = multicast $ sampleOn_ houseEvt uploadedEvt
        createdEvt  = multicast $ runAPIEvent apiCfg $ createManual leadId <$> toCreateEvt

        readyEvt = runAPIEvent apiCfg $ const (repeatCheckUntilReady leadId) <$> createdEvt

    in (const UploadingFiles  <$> mFilesEvt)   <|>
       (const CreatingHouse   <$> toCreateEvt) <|>
       (const WaitingForReady <$> createdEvt)  <|>
       (const Finished        <$> readyEvt)


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
        conf     = def # _sizeDyn     .~ (editor ^. _sizeDyn)
                       # _showSaveDyn .~ step false (res ^. _hasHouse)
                       # _showResetDyn .~ step false ((==) Tracing <$> (res ^. _tracerMode))
    uiEvts <- runMainWidgetInNode parentEl $ houseBuilderUI conf

    void $ subscribe (const unit <$> uiEvts ^. _save) toExp
    void $ subscribe (const unit <$> uiEvts ^. _reset) toStopTracing

    pure $ res # _editorOp .~ (const Close <$> uiEvts ^. _close)
