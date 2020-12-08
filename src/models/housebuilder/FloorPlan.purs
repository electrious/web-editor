module Model.HouseBuilder.FloorPlan (FloorPlan(..), FloorPlanOp(..)) where

import Prelude

import Custom.Mesh (TappableMesh)
import Data.Lens (view)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_id, _polygon)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic)
import Model.ActiveMode (ActiveMode(..))
import Model.Polygon (Polygon)
import Model.UUID (class HasUUID)
import Rendering.Node (localEnv)
import Rendering.NodeRenderable (class NodeRenderable, render)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial, setOpacity, setTransparent)

-- | a FloorPlan represent a house part with 2D polygon and height
newtype FloorPlan = FloorPlan {
    id      :: UUID,
    polygon :: Polygon,
    height  :: Meter
}

derive instance newtypeFloorPlan :: Newtype FloorPlan _
derive instance eqFloorPlan :: Eq FloorPlan
instance hasUUIDFloorPlan :: HasUUID FloorPlan where
    idLens = _id

-- | mesh material for active floor plan
activeMat :: MeshBasicMaterial
activeMat = unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xffff88
    setTransparent true mat
    setopacity 0.9 mat
    pure mat

-- | mesh material for inactive floor plan
inactiveMat :: MeshBasicMaterial
inactiveMat = unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xffffbb
    setTransparent true mat
    setOpacity 0.7 mat
    pure mat

-- | get the right material based on active mode of floor plan
floorPlanMaterial :: ActiveMode -> MeshBasicMaterial
floorPlanMaterial Active   = activeMat
floorPlanMaterial Inactive = inactiveMat

instance nodeRenderableFloorPlan :: NodeRenderable (Dynamic ActiveMode) FloorPlan TappableMesh where
    render = localEnv (map floorPlanMaterial) <<< render <<< view _polygon

-- | operations applied to FloorPlans
data FloorPlanOp = FPOCreate FloorPlan
                 | FPODelete UUID
                 | FPOUpdate FloorPlan

derive instance eqFloorPlanOp :: Eq FloorPlanOp
