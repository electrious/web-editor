module SmartHouse.HouseBuilder (buildHouse, HouseBuilderConfig) where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Custom.Mesh (TapMouseMesh)
import Data.Default (class Default, def)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Meter (meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.UUID (UUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_active, _height, _leadId, _mouseMove, _name, _tapped, _width)
import Editor.Editor (Editor)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (latestEvt, sampleDyn)
import FRP.Event (Event)
import FRP.Event.Extra (anyEvt, performEvent)
import Math.Angle (degree)
import Model.SmartHouse.House (House, createHouseFrom, houseTapped, renderHouse)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _size, _texture, mkHouseTextureInfo)
import Model.UUID (idLens)
import Rendering.DynamicNode (dynamic)
import Rendering.Node (Node, fixNodeDWith, getEnv, localEnv, mkNodeEnv, node, runNode, tapMouseMesh)
import Rendering.TextureLoader (loadTextureFromUrl)
import SmartHouse.HouseTracer (traceHouse)
import Three.Core.Geometry (mkPlaneGeometry)
import Three.Core.Material (mkMeshBasicMaterialWithTexture)
import Three.Loader.TextureLoader (Texture, clampToEdgeWrapping, repeatWrapping, setRepeat, setWrapS, setWrapT)

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


newtype HousesState = HousesState {
    houses :: UUIDMap House,
    active :: Maybe UUID
    }

derive instance newtypeHousesState :: Newtype HousesState _
instance defaultHousesState :: Default HousesState where
    def = HousesState {
        houses : M.empty,
        active : Nothing
        }
_houses :: forall t a r. Newtype t { houses :: a | r } => Lens' t a
_houses = _Newtype <<< prop (SProxy :: SProxy "houses")

addHouse :: House -> HousesState -> HousesState
addHouse h s = s # _houses %~ M.insert (h ^. idLens) h
                 # _active .~ Just (h ^. idLens)

activateRoof :: Maybe UUID -> HousesState -> HousesState
activateRoof u s = s # _active .~ u

renderHousesState :: HousesState -> Node HouseTextureInfo (Event UUID)
renderHousesState s = anyEvt <<< map houseTapped <$> traverse render (s ^. _houses)
    where render h = renderHouse (s ^. _active == Just (h ^. idLens)) h

createHouseBuilder :: Node HouseBuilderConfig Unit
createHouseBuilder = node (def # _name .~ "house-builder") $ do
    lId <- view _leadId <$> getEnv
    t <- liftEffect $ loadHouseTexture lId
    let tInfo = mkHouseTextureInfo t (def # _width .~ meter 100.0
                                          # _height .~ meter 46.5)
    
    fixNodeDWith def \stDyn -> do
        -- add helper plane that accepts tap and drag events
        helper <- mkHelperPlane tInfo

        -- render all houses
        actEvt <- localEnv (const tInfo) $ latestEvt <$> dynamic (renderHousesState <$> stDyn)

        let cfg        = def # _mouseMove .~ helper ^. _mouseMove
            deactEvt   = const Nothing <$> helper ^. _tapped
            actRoofEvt = (Just <$> actEvt) <|> deactEvt
            
        floorPlanEvt <- localEnv (const cfg) traceHouse
        let houseEvt = performEvent $ createHouseFrom (degree 30.0) <$> floorPlanEvt

            newStEvt = sampleDyn stDyn $ (addHouse <$> houseEvt)
                                     <|> (activateRoof <$> actRoofEvt)

        pure { input: newStEvt, output : unit }


-- | external API to build a 3D house for 2D lead
buildHouse :: Editor -> HouseBuilderConfig -> Effect Unit
buildHouse editor cfg = void $ runNode createHouseBuilder $ mkNodeEnv editor cfg
