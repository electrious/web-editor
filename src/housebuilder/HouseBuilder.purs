module HouseBuilder.HouseBuilder (buildHouse, HouseBuilderConfig) where

import Prelude

import Custom.Mesh (TapMouseMesh)
import Data.Default (class Default, def)
import Data.Foldable (traverse_)
import Data.Lens (view, (.~), (^.))
import Data.List (List, singleton)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_leadId, _mouseMove, _name)
import Editor.Editor (Editor)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (step)
import FRP.Event (subscribe)
import FRP.Event.Extra (performEvent)
import HouseBuilder.FloorPlanBuilder (_canEdit)
import HouseBuilder.PolyGeometry (mkPolyGeometry)
import Math.Angle (degree)
import Model.Polygon (Polygon, counterClockPoly)
import Rendering.DynamicNode (eventNode_)
import Rendering.Node (Node, fixNodeE, getEnv, localEnv, mesh, mkNodeEnv, node, runNode, tapMouseMesh)
import Rendering.TextureLoader (loadTextureFromUrl)
import SmartHouse.Algorithm.Skeleton (skeletonize)
import SmartHouse.HouseTracer (traceHouse)
import Three.Core.Geometry (mkPlaneGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial, mkMeshBasicMaterialWithTexture)
import Three.Loader.TextureLoader (clampToEdgeWrapping, repeatWrapping, setRepeat, setWrapS, setWrapT)
import Three.Math.Vector (Vector3)

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
        let polysEvt = performEvent $ skeletonize (degree 70.0) <<< singleton <<< counterClockPoly <$> floorPlanEvt
        eventNode_ $ renderRoofPolys <$> polysEvt
        
        pure { input: nModeEvt, output : unit}


-- | external API to build a 3D house for 2D lead
buildHouse :: Editor -> HouseBuilderConfig -> Effect Unit
buildHouse editor cfg = void $ runNode createHouseBuilder $ mkNodeEnv editor cfg


renderRoofPolys :: forall e. List (Polygon Vector3) -> Node e Unit
renderRoofPolys = traverse_ renderRoofPoly

roofMaterial :: MeshBasicMaterial
roofMaterial = unsafePerformEffect $ mkMeshBasicMaterial 0xaabbcc

renderRoofPoly :: forall e. Polygon Vector3 -> Node e Unit
renderRoofPoly poly = do
    geo <- liftEffect $ mkPolyGeometry poly
    void $ mesh (def # _name .~ "roof") geo roofMaterial
