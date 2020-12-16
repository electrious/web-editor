module Model.HouseBuilder.FloorPlan (FloorPlan(..), newFloorPlan, FloorPlanOp(..)) where

import Prelude

import Custom.Mesh (TappableMesh)
import Data.Default (class Default, def)
import Data.Lens ((^.), (.~))
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.UUID (UUID, emptyUUID, genUUID)
import Editor.Common.Lenses (_height, _id, _name, _polygon)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic)
import Model.ActiveMode (ActiveMode(..))
import Model.Polygon (Polygon, _polyVerts, polygonAround)
import Model.UUID (class HasUUID)
import Rendering.Node (leaf, localEnv, mesh')
import Rendering.NodeRenderable (class NodeRenderable, render)
import Three.Core.Geometry (_depth, mkExtrudeGeometry, mkShape)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial, setOpacity, setTransparent)
import Three.Math.Vector (Vector2)

-- | a FloorPlan represent a house part with 2D polygon and height
newtype FloorPlan = FloorPlan {
    id      :: UUID,
    polygon :: Polygon,
    height  :: Meter
}

derive instance newtypeFloorPlan :: Newtype FloorPlan _
derive instance eqFloorPlan      :: Eq FloorPlan
instance hasUUIDFloorPlan        :: HasUUID FloorPlan where
    idLens = _id
instance defaultFloorPlan        :: Default FloorPlan where
    def = FloorPlan {
        id      : emptyUUID,
        polygon : def,
        height  : def
        }

newFloorPlan :: Vector2 -> Effect FloorPlan
newFloorPlan p = do
    i <- genUUID
    pure $ FloorPlan {
        id      : i,
        polygon : polygonAround p,
        height  : meter 0.0
        }

-- | mesh material for active floor plan
activeMat :: MeshBasicMaterial
activeMat = unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xffff88
    setTransparent true mat
    setOpacity 0.9 mat
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
    render fp = do
        let poly = fp ^. _polygon
            h    = meterVal $ fp ^. _height
        m <- localEnv (map floorPlanMaterial) $ render poly

        when (h > 0.0) do
            shp <- liftEffect $ mkShape $ poly ^. _polyVerts
            geo <- liftEffect $ mkExtrudeGeometry shp $ def # _depth .~ h
            mat <- liftEffect $ mkMeshBasicMaterial 0xffeeff

            mesh' (def # _name .~ "floor-body") geo mat leaf
        
        pure m

-- | operations applied to FloorPlans
data FloorPlanOp = FPOCreate FloorPlan
                 | FPODelete UUID
                 | FPOUpdate FloorPlan

derive instance eqFloorPlanOp :: Eq FloorPlanOp
