module SmartHouse.HouseBuilder (buildHouse, HouseBuilderConfig, HouseBuilt, _filesExported, _housesExported, _hasHouse) where

import Prelude hiding (degree)

import API (APIConfig, runAPI)
import API.Image (_link, getImageMeta)
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Custom.Mesh (TapMouseMesh)
import Data.Array (fromFoldable)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as M
import Data.Maybe (Maybe(..), isNothing)
import Data.Meter (meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.UUID (UUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_apiConfig, _deleted, _height, _leadId, _modeDyn, _mouseMove, _name, _parent, _tapped, _updated, _width)
import Editor.Editor (Editor, _sizeDyn, setMode)
import Editor.EditorMode as EditorMode
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, step)
import FRP.Event (Event, create, keepLatest, sampleOn, sampleOn_, subscribe)
import FRP.Event.Extra (delay, multicast, performEvent)
import Math.Angle (degree)
import Model.ActiveMode (ActiveMode(..), fromBoolean)
import Model.SmartHouse.House (House, HouseNode, HouseOp(..), JSHouses(..), createHouseFrom, exportHouse, houseTapped)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _size, _texture, mkHouseTextureInfo)
import Model.UUID (idLens)
import OBJExporter (MeshFiles, exportObject)
import Rendering.DynamicNode (eventNode)
import Rendering.Node (Node, fixNodeDWith, fixNodeEWith, getEnv, getParent, localEnv, mkNodeEnv, node, runNode, tapMouseMesh)
import Rendering.TextureLoader (loadTextureFromUrl)
import SmartHouse.BuilderMode (BuilderMode(..))
import SmartHouse.HouseEditor (editHouse)
import SmartHouse.HouseTracer (traceHouse)
import SmartHouse.UI (_showSaveDyn, houseBuilderUI)
import Specular.Dom.Widget (runMainWidgetInNode)
import Three.Core.Geometry (mkPlaneGeometry)
import Three.Core.Material (mkMeshBasicMaterialWithTexture)
import Three.Loader.TextureLoader (Texture, clampToEdgeWrapping, repeatWrapping, setRepeat, setWrapS, setWrapT)
import UI.ButtonPane (ButtonClicked(..))
import UI.EditorUIOp (EditorUIOp(..))
import UI.RoofEditorUI (_editorOp)
import Unsafe.Coerce (unsafeCoerce)
import Util (foldEvtWith)

newtype HouseBuilderConfig = HouseBuilderConfig {
    leadId    :: Int,
    apiConfig :: APIConfig
}

derive instance newtypeHouseBuilderConfig :: Newtype HouseBuilderConfig _
instance defaultHouseBuilderConfig :: Default HouseBuilderConfig where
    def = HouseBuilderConfig { leadId : 0, apiConfig : def }

newtype HouseBuilt = HouseBuilt {
    filesExported  :: Event MeshFiles,
    housesExported :: Event JSHouses,
    hasHouse       :: Event Boolean,
    editorOp       :: Event EditorUIOp
    }

derive instance newtypeHouseBuilt :: Newtype HouseBuilt _
instance defaultHouseBuilt :: Default HouseBuilt where
    def = HouseBuilt {
        filesExported  : empty,
        housesExported : empty,
        hasHouse       : empty,
        editorOp       : empty
        }

_filesExported :: forall t a r. Newtype t { filesExported :: a | r } => Lens' t a
_filesExported = _Newtype <<< prop (SProxy :: SProxy "filesExported")

_housesExported :: forall t a r. Newtype t { housesExported :: a | r } => Lens' t a
_housesExported = _Newtype <<< prop (SProxy :: SProxy "housesExported")

_hasHouse :: forall t a r. Newtype t { hasHouse :: a | r } => Lens' t a
_hasHouse = _Newtype <<< prop (SProxy :: SProxy "hasHouse")

compactHouseBuilt :: Event HouseBuilt -> HouseBuilt
compactHouseBuilt e = def # _filesExported  .~ keepLatest (view _filesExported <$> e)
                          # _housesExported .~ keepLatest (view _housesExported <$> e)
                          # _hasHouse       .~ keepLatest (view _hasHouse <$> e)
                          # _editorOp       .~ keepLatest (view _editorOp <$> e)


loadHouseTexture :: String -> Effect Texture
loadHouseTexture imgUrl = do
    let t = loadTextureFromUrl imgUrl
    setWrapS clampToEdgeWrapping t
    setWrapT repeatWrapping t
    setRepeat 1.0 1.0 t

    pure t


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
          
          render h = editHouse (getMode h <$> actHouseDyn <*> modeDyn) h


tracerMode :: Maybe UUID -> BuilderMode -> ActiveMode
tracerMode _ Showing  = Inactive
tracerMode h Building = fromBoolean $ isNothing h

createHouseBuilder :: Event Unit -> Node HouseBuilderConfig HouseBuilt
createHouseBuilder exportEvt = node (def # _name .~ "house-builder") $ do
    cfg <- getEnv
    let lId = cfg ^. _leadId

    -- load image meta info
    imgEvt <- liftEffect $ runAPI (getImageMeta $ def # _leadId .~ lId) (cfg ^. _apiConfig)

    -- load texture image
    let tEvt = performEvent $ loadHouseTexture <<< view _link <$> imgEvt
        s = def # _width  .~ meter 100.0
                # _height .~ meter 46.5
        tInfoEvt = flip mkHouseTextureInfo s <$> tEvt
    evt <- eventNode $ builderForHouse exportEvt <$> tInfoEvt
    pure $ compactHouseBuilt evt

builderForHouse :: Event Unit -> HouseTextureInfo -> Node HouseBuilderConfig HouseBuilt
builderForHouse exportEvt tInfo =
    fixNodeEWith def \hdEvt ->
        fixNodeDWith Nothing \actHouseDyn ->
            fixNodeDWith Building \modeDyn -> do
                pNode <- getParent
                
                -- add helper plane that accepts tap and drag events
                helper <- mkHelperPlane tInfo

                -- render all houses
                let houseToRenderEvt = compact $ view _housesToRender <$> hdEvt
                nodesEvt <- localEnv (const tInfo) $ eventNode (renderHouseDict actHouseDyn modeDyn <$> houseToRenderEvt)

                let deactEvt    = multicast $ const Nothing <$> helper ^. _tapped
                    actEvt      = keepLatest $ getActivated <$> nodesEvt
                    actHouseEvt = (Just <$> actEvt) <|> deactEvt
                
                floorPlanEvt <- traceHouse $ def # _modeDyn   .~ (tracerMode <$> actHouseDyn <*> modeDyn)
                                                 # _mouseMove .~ helper ^. _mouseMove
                let houseEvt = performEvent $ createHouseFrom (degree 30.0) <$> floorPlanEvt
                    addHouseEvt = HouseOpCreate <$> houseEvt
                    updHouseEvt = keepLatest (getHouseUpd <$> nodesEvt)

                    opEvt = addHouseEvt <|> updHouseEvt

                    newHdEvt = multicast $ sampleOn hdEvt $ applyHouseOp <$> opEvt

                    modeEvt = (const Showing <$> exportEvt) <|> 
                              (const Building <$> delay 30 exportEvt)
                    toExpEvt = delay 15 exportEvt
                    res = def # _hasHouse       .~ (hasHouse <$> hdEvt)
                              # _filesExported  .~ performEvent (const (exportObject pNode) <$> toExpEvt)
                              # _housesExported .~ (exportHouses <$> sampleOn_ hdEvt toExpEvt)

                pure { input: modeEvt, output: { input: actHouseEvt, output : { input: newHdEvt, output : res } } }
                

-- | external API to build a 3D house for 2D lead
buildHouse :: Editor -> HouseBuilderConfig -> Effect HouseBuilt
buildHouse editor cfg = do
    setMode editor EditorMode.HouseBuilding
    
    { event: expEvt, push: toExp } <- create

    res <- fst <$> runNode (createHouseBuilder expEvt) (mkNodeEnv editor cfg)
    let parentEl = unsafeCoerce $ editor ^. _parent
        conf     = def # _sizeDyn     .~ (editor ^. _sizeDyn)
                       # _showSaveDyn .~ step false (res ^. _hasHouse)
    uiEvts <- runMainWidgetInNode parentEl $ houseBuilderUI conf
    
    void $ subscribe (const unit <$> filter ((==) BCSave) uiEvts) toExp

    pure $ res # _editorOp .~ (const Close <$> filter ((==) BCClose) uiEvts)
