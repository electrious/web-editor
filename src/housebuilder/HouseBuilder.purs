module HouseBuilder.HouseBuilder (buildHouse, HouseBuilderConfig) where

import Prelude hiding (degree)

import Custom.Mesh (TapMouseMesh)
import Data.Default (class Default, def)
import Data.Foldable (traverse_)
import Data.Lens (view, (.~), (^.))
import Data.Map as M
import Data.Meter (meter, meterVal)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_height, _leadId, _mouseMove, _name, _width)
import Editor.Editor (Editor)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (sampleDyn)
import FRP.Event.Extra (performEvent)
import Math.Angle (degree)
import Model.SmartHouse.House (createHouseFrom, renderHouse)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _size, _texture, mkHouseTextureInfo)
import Model.UUID (idLens)
import Rendering.DynamicNode (dynamic_)
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


createHouseBuilder :: Node HouseBuilderConfig Unit
createHouseBuilder = node (def # _name .~ "house-builder") $ do
    lId <- view _leadId <$> getEnv
    t <- liftEffect $ loadHouseTexture lId
    let tInfo = mkHouseTextureInfo t (def # _width .~ meter 100.0
                                          # _height .~ meter 46.5)
    
    fixNodeDWith M.empty \housesDyn -> do
        -- add helper plane that accepts tap and drag events
        helper <- mkHelperPlane tInfo

        -- render all houses
        localEnv (const tInfo) $ dynamic_ $ traverse_ renderHouse <$> housesDyn

        let cfg = def # _mouseMove .~ helper ^. _mouseMove

            --bgTapEvt = const unit <$> helper ^. _tapped
        
        floorPlanEvt <- localEnv (const cfg) traceHouse
        let houseEvt = performEvent $ createHouseFrom (degree 30.0) <$> floorPlanEvt

            f h = M.insert (h ^. idLens) h
            newHsEvt = sampleDyn housesDyn $ f <$> houseEvt

        pure { input: newHsEvt, output : unit }


-- | external API to build a 3D house for 2D lead
buildHouse :: Editor -> HouseBuilderConfig -> Effect Unit
buildHouse editor cfg = void $ runNode createHouseBuilder $ mkNodeEnv editor cfg
