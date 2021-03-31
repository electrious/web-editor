module HouseBuilder.HouseBuilder (buildHouse, HouseBuilderConfig) where

import Prelude

import Custom.Mesh (TapMouseMesh)
import Data.Default (class Default, def)
import Data.Foldable (traverse_)
import Data.Lens (view, (.~), (^.))
import Data.List (List, concatMap, singleton)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_leadId, _mouseMove, _name)
import Editor.Editor (Editor)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Dynamic (step)
import FRP.Event (subscribe)
import FRP.Event.Extra (performEvent)
import HouseBuilder.FloorPlanBuilder (_canEdit)
import Rendering.DynamicNode (eventNode_)
import Rendering.Node (Node, fixNodeE, getEnv, localEnv, mkNodeEnv, node, runNode, tapMouseMesh)
import Rendering.TextureLoader (loadTextureFromUrl)
import SmartHouse.Algorithm.Skeleton (skeletonize)
import SmartHouse.HouseTracer (renderLine, traceHouse)
import Smarthouse.Algorithm.Subtree (Subtree, treeLines)
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
createHouseBuilder = node (def # _name .~ "house-builder") $
    fixNodeE \newModeEvt -> do
        -- add helper plane that accepts tap and drag events
        helper <- mkHelperPlane

        let modeDyn = step AddFloorPlan newModeEvt
            cfg = def # _mouseMove .~ helper ^. _mouseMove
                      # _canEdit   .~ ((==) AddFloorPlan <$> modeDyn)

            --bgTapEvt = const unit <$> helper ^. _tapped
    
        floorPlanEvt <- localEnv (const cfg) $ traceHouse

        _ <- liftEffect $ subscribe floorPlanEvt (show >>> log)
        let nModeEvt = const AddRoofs <$> floorPlanEvt

        -- calculate skeletons
        let skeletons = performEvent $ skeletonize <<< singleton <$> floorPlanEvt
        eventNode_ $ renderSkeletons <$> skeletons
        
        pure { input: nModeEvt, output : unit}


-- | external API to build a 3D house for 2D lead
buildHouse :: Editor -> HouseBuilderConfig -> Effect Unit
buildHouse editor cfg = void $ runNode createHouseBuilder $ mkNodeEnv editor cfg


renderSkeletons :: forall e. List Subtree -> Node e Unit
renderSkeletons trees = traverse_ renderLine $ concatMap treeLines trees
