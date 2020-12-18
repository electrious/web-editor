module HouseBuilder.HouseBuilder (buildHouse, HouseBuilderConfig) where

import Prelude hiding (add)

import Control.Plus (empty)
import Custom.Mesh (TapDragMesh)
import Data.Default (class Default, def)
import Data.Lens (view, (.~))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), snd)
import Editor.Common.Lenses (_leadId, _mouseMove, _name)
import Editor.Editor (Editor)
import Editor.SceneEvent (SceneMouseMoveEvent, makeMouseMove, stopMouseMove)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (step)
import FRP.Event (Event, makeEvent)
import FRP.Event.Extra (multicast)
import HouseBuilder.FloorPlanBuilder (_canEdit, buildFloorPlan)
import Rendering.Node (Node, getEnv, leaf, localEnv, mkNodeEnv, node, runNode, tapDragMesh)
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

mkHelperPlane :: Node HouseBuilderConfig (Tuple TapDragMesh (Event SceneMouseMoveEvent))
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

    m <- snd <$> tapDragMesh (def # _name .~ "helper-plane") geo mat leaf
    let mouseEvt = multicast $ makeEvent \k -> do
            makeMouseMove m k
            pure (stopMouseMove m)
            
    pure $ Tuple m mouseEvt

createHouseBuilder :: Node HouseBuilderConfig Unit
createHouseBuilder = node (def # _name .~ "house-builder") do
    -- add helper plane that accepts tap and drag events
    Tuple plane mouseEvt <- mkHelperPlane

    let modeDyn = step AddFloorPlan empty
        cfg = def # _mouseMove .~ mouseEvt
                  # _canEdit   .~ ((==) AddFloorPlan <$> modeDyn)
    
    floorPlanEvt <- localEnv (const cfg) buildFloorPlan
    
    pure unit


-- | external API to build a 3D house for 2D lead
buildHouse :: Editor -> HouseBuilderConfig -> Effect Unit
buildHouse editor cfg = void $ runNode createHouseBuilder $ mkNodeEnv editor cfg
