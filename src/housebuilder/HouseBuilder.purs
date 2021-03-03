module HouseBuilder.HouseBuilder (buildHouse, HouseBuilderConfig) where

import Custom.Mesh (TapMouseMesh)
import Data.Default (class Default, def)
import Data.Lens (view, (.~), (^.))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_leadId, _mouseMove, _name)
import Editor.Editor (Editor)
import Effect (Effect)
import Effect.Class (liftEffect)
import HouseBuilder.FloorPlanBuilder (_canEdit)
import Prelude (class Eq, Unit, bind, const, discard, pure, show, unit, void, (#), ($), (<$>), (<>), (==))
import Rendering.Node (Node, getEnv, localEnv, mkNodeEnv, node, runNode, tapMouseMesh)
import Rendering.TextureLoader (loadTextureFromUrl)
import SmartHouse.HouseTracer (traceHouse)
import Three.Core.Geometry (mkPlaneGeometry)
import Three.Core.Material (mkMeshBasicMaterialWithTexture)
import Three.Loader.TextureLoader (clampToEdgeWrapping, repeatWrapping, setRepeat, setWrapS, setWrapT)

-- represent the state of the builder
data BuilderMode = AddFloorPlan  -- Add and edit FloorPlans
                 | AddRoofs      -- Add Roof ridges and plates

derive instance eqBuilderMode :: Eq BuilderMode


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
createHouseBuilder = node (def # _name .~ "house-builder") do
    -- add helper plane that accepts tap and drag events
    helper <- mkHelperPlane

    let modeDyn = pure AddFloorPlan
        cfg = def # _mouseMove .~ helper ^. _mouseMove
                  # _canEdit   .~ ((==) AddFloorPlan <$> modeDyn)

        --bgTapEvt = const unit <$> helper ^. _tapped
    
    floorPlanEvt <- localEnv (const cfg) $ traceHouse
    
    pure unit


-- | external API to build a 3D house for 2D lead
buildHouse :: Editor -> HouseBuilderConfig -> Effect Unit
buildHouse editor cfg = void $ runNode createHouseBuilder $ mkNodeEnv editor cfg
