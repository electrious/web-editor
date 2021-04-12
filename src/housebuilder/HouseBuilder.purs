module HouseBuilder.HouseBuilder (buildHouse, HouseBuilderConfig) where

import Prelude hiding (degree)

import Custom.Mesh (TapMouseMesh)
import Data.Default (class Default, def)
import Data.Foldable (traverse_)
import Data.Lens (view, (.~), (^.))
import Data.Map as M
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_leadId, _mouseMove, _name)
import Editor.Editor (Editor)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (sampleDyn)
import FRP.Event.Extra (performEvent)
import Math.Angle (degree)
import Model.SmartHouse.House (createHouseFrom, renderHouse)
import Model.UUID (idLens)
import Rendering.DynamicNode (dynamic_)
import Rendering.Node (Node, fixNodeDWith, getEnv, localEnv, mkNodeEnv, node, runNode, tapMouseMesh)
import Rendering.TextureLoader (loadTextureFromUrl)
import SmartHouse.HouseTracer (traceHouse)
import Three.Core.Geometry (mkPlaneGeometry)
import Three.Core.Material (mkMeshBasicMaterialWithTexture)
import Three.Loader.TextureLoader (clampToEdgeWrapping, repeatWrapping, setRepeat, setWrapS, setWrapT)

newtype HouseBuilderConfig = HouseBuilderConfig {
    leadId :: Int
}

derive instance newtypeHouseBuilderConfig :: Newtype HouseBuilderConfig _
instance defaultHouseBuilderConfig :: Default HouseBuilderConfig where
    def = HouseBuilderConfig { leadId : 0 }


-- | get 2D image url for a lead
imageUrlForLead :: Int -> String
imageUrlForLead l = "https://s3.eu-west-1.amazonaws.com/data.electrious.com/leads/" <> show l <> "/manual.jpg"

mkHelperPlane :: Node HouseBuilderConfig TapMouseMesh
mkHelperPlane = do
    lId <- view _leadId <$> getEnv
    let img = imageUrlForLead lId

    geo <- liftEffect $ mkPlaneGeometry 100.0 46.5 10 10
    let t = loadTextureFromUrl img
    liftEffect do
        setWrapS clampToEdgeWrapping t
        setWrapT repeatWrapping t
        setRepeat 1.0 1.0 t
    mat <- liftEffect $ mkMeshBasicMaterialWithTexture t

    tapMouseMesh (def # _name .~ "helper-plane") geo mat


createHouseBuilder :: Node HouseBuilderConfig Unit
createHouseBuilder = node (def # _name .~ "house-builder") $
    fixNodeDWith M.empty \housesDyn -> do
        -- add helper plane that accepts tap and drag events
        helper <- mkHelperPlane

        -- render all houses
        dynamic_ $ traverse_ renderHouse <$> housesDyn

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
