module HouseBuilder.HouseBuilder (buildHouse, HouseBuilderConfig) where

import Custom.Mesh (TapMouseMesh)
import Prelude (class Eq, Unit, bind, const, discard, pure, show, unit, void, (#), ($), (<$>), (<>), (==))

import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Lens (view, (.~), (^.))
import Data.Newtype (class Newtype)
import Data.Tuple (snd)
import Editor.Common.Lenses (_leadId, _mouseMove, _name)
import Editor.Editor (Editor)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (step)
import HouseBuilder.FloorPlanBuilder (_canEdit, buildFloorPlan)
import Rendering.Node (Node, getEnv, leaf, localEnv, mkNodeEnv, node, runNode, tapMouseMesh)
import Rendering.TextureLoader (loadTextureFromUrl)
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

    snd <$> tapMouseMesh (def # _name .~ "helper-plane") geo mat leaf

createHouseBuilder :: Node HouseBuilderConfig Unit
createHouseBuilder = node (def # _name .~ "house-builder") do
    -- add helper plane that accepts tap and drag events
    planEvt <- mkHelperPlane

    let modeDyn = step AddFloorPlan empty
        cfg = def # _mouseMove .~ planEvt ^. _mouseMove
                  # _canEdit   .~ ((==) AddFloorPlan <$> modeDyn)
    
    floorPlanEvt <- localEnv (const cfg) buildFloorPlan
    
    pure unit


-- | external API to build a 3D house for 2D lead
buildHouse :: Editor -> HouseBuilderConfig -> Effect Unit
buildHouse editor cfg = void $ runNode createHouseBuilder $ mkNodeEnv editor cfg
