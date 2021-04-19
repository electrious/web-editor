module SmartHouse.HouseBuilder (buildHouse, HouseBuilderConfig) where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Custom.Mesh (TapMouseMesh)
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
import Data.Meter (meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.UUID (UUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_deleted, _height, _leadId, _modeDyn, _mouseMove, _name, _tapped, _updated, _width)
import Editor.Editor (Editor)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event, keepLatest, sampleOn)
import FRP.Event.Extra (performEvent)
import Math.Angle (degree)
import Model.ActiveMode (ActiveMode(..), fromBoolean)
import Model.SmartHouse.House (House, HouseNode, HouseOp(..), createHouseFrom, houseTapped)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _size, _texture, mkHouseTextureInfo)
import Model.UUID (idLens)
import Rendering.DynamicNode (eventNode)
import Rendering.Node (Node, fixNodeDWith, fixNodeEWith, getEnv, localEnv, mkNodeEnv, node, runNode, tapMouseMesh)
import Rendering.TextureLoader (loadTextureFromUrl)
import SmartHouse.HouseEditor (editHouse)
import SmartHouse.HouseTracer (traceHouse)
import Three.Core.Geometry (mkPlaneGeometry)
import Three.Core.Material (mkMeshBasicMaterialWithTexture)
import Three.Loader.TextureLoader (Texture, clampToEdgeWrapping, repeatWrapping, setRepeat, setWrapS, setWrapT)
import Util (foldEvtWith)

newtype HouseBuilderConfig = HouseBuilderConfig {
    leadId :: Int
}

derive instance newtypeHouseBuilderConfig :: Newtype HouseBuilderConfig _
instance defaultHouseBuilderConfig :: Default HouseBuilderConfig where
    def = HouseBuilderConfig { leadId : 0 }


-- | get 2D image url for a lead
imageUrlForLead :: Int -> String
imageUrlForLead l = "https://s3.eu-west-1.amazonaws.com/data.electrious.com/leads/" <> show l <> "/manual.jpg"

loadHouseTexture :: Int -> Effect Texture
loadHouseTexture lId = do
    let img = imageUrlForLead lId
        t   = loadTextureFromUrl img
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

-- mark all houses to be rendered
renderAll :: HouseDictData -> HouseDictData
renderAll s = s # _housesToRender .~ Just (s ^. _houses)

-- | update the HouseDictData with house operations
applyHouseOp :: HouseOp -> HouseDictData -> HouseDictData
applyHouseOp (HouseOpCreate house) d = renderAll $ d # _houses %~ M.insert (house ^. idLens) house
applyHouseOp (HouseOpDelete hid)   d = renderAll $ d # _houses %~ M.delete hid
applyHouseOp (HouseOpUpdate house) d = d # _houses %~ M.insert (house ^. idLens) house
                                         # _housesToRender .~ Nothing


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

renderHouseDict :: Dynamic (Maybe UUID) -> HouseDict -> Node HouseTextureInfo (UUIDMap HouseNode)
renderHouseDict actHouseDyn houses = traverse render houses
    where getMode h (Just i) | h ^. idLens == i = Active
                             | otherwise        = Inactive
          getMode h Nothing                     = Inactive
          
          render h = editHouse (getMode h <$> actHouseDyn) h

createHouseBuilder :: Node HouseBuilderConfig Unit
createHouseBuilder = node (def # _name .~ "house-builder") $ do
    lId <- view _leadId <$> getEnv
    t <- liftEffect $ loadHouseTexture lId
    let tInfo = mkHouseTextureInfo t (def # _width  .~ meter 100.0
                                          # _height .~ meter 46.5)
    
    fixNodeEWith def \hdEvt ->
        fixNodeDWith Nothing \actRoofDyn -> do
            -- add helper plane that accepts tap and drag events
            helper <- mkHelperPlane tInfo

            -- render all houses
            let houseToRenderEvt = compact $ view _housesToRender <$> hdEvt
            nodesEvt <- localEnv (const tInfo) $ eventNode (renderHouseDict actRoofDyn <$> houseToRenderEvt)

            let deactEvt   = const Nothing <$> helper ^. _tapped
                actEvt     = keepLatest $ getActivated <$> nodesEvt
                actRoofEvt = (Just <$> actEvt) <|> deactEvt
            
            floorPlanEvt <- traceHouse $ def # _modeDyn   .~ (fromBoolean <<< isNothing <$> actRoofDyn)
                                             # _mouseMove .~ helper ^. _mouseMove
            let houseEvt = performEvent $ createHouseFrom (degree 30.0) <$> floorPlanEvt
                addHouseEvt = HouseOpCreate <$> houseEvt
                updHouseEvt = keepLatest (getHouseUpd <$> nodesEvt)

                opEvt = addHouseEvt <|> updHouseEvt

                newHdEvt = sampleOn hdEvt $ applyHouseOp <$> opEvt

            pure { input: actRoofEvt, output : { input: newHdEvt, output : unit } }


-- | external API to build a 3D house for 2D lead
buildHouse :: Editor -> HouseBuilderConfig -> Effect Unit
buildHouse editor cfg = void $ runNode createHouseBuilder $ mkNodeEnv editor cfg
