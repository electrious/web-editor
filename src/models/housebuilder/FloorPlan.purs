module Model.HouseBuilder.FloorPlan (FloorPlan(..), newFloorPlan, FloorPlanOp(..), floorPlanTop) where

import Prelude

import Custom.Mesh (TappableMesh)
import Data.Default (class Default, def)
import Data.Lens (view, (.~), (^.))
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.UUID (UUID, emptyUUID, genUUID)
import Editor.Common.Lenses (_height, _id, _name, _polygon, _position)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic)
import Model.ActiveMode (ActiveMode(..))
import Model.Polygon (class IsPolygon, Polygon, _polyVerts, polygonAround)
import Model.UUID (class HasUUID)
import Rendering.Node (localEnv, mesh, node)
import Rendering.NodeRenderable (class NodeRenderable, render)
import Three.Core.Geometry (_bevelEnabled, _depth, mkExtrudeGeometry, mkShape)
import Three.Core.Material (MeshBasicMaterial, doubleSide, mkMeshBasicMaterial, mkMeshPhongMaterial, setDepthWrite, setOpacity, setSide, setTransparent)
import Three.Math.Vector (Vector2, mkVec3)

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
instance isPolygonFloorPlan :: IsPolygon FloorPlan where
    toPolygon = view _polygon

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

-- | calculate the floor plan's top height used for rendering
floorPlanTop :: FloorPlan -> Meter
floorPlanTop fp = fp ^. _height + meter 0.02

instance nodeRenderableFloorPlan :: NodeRenderable (Dynamic ActiveMode) FloorPlan TappableMesh where
    render fp = do
        let poly = fp ^. _polygon
            h    = meterVal $ fp ^. _height
            pos  = mkVec3 0.0 0.0 (meterVal $ floorPlanTop fp)
        m <- node (def # _position .~ pure pos) $ localEnv (map floorPlanMaterial) $ render poly

        when (h > 0.0) do
            shp <- liftEffect $ mkShape $ poly ^. _polyVerts
            geo <- liftEffect $ mkExtrudeGeometry shp $ def # _depth .~ h
                                                            # _bevelEnabled .~ false
            mat <- liftEffect $ mkMeshPhongMaterial 0x999999
            liftEffect do
                setSide doubleSide mat
                setTransparent true mat
                setOpacity 0.5 mat
                setDepthWrite false mat

            void $ mesh (def # _name .~ "floor-body") geo mat
        
        pure m

-- | operations applied to FloorPlans
data FloorPlanOp = FPOCreate FloorPlan
                 | FPODelete UUID
                 | FPOUpdate FloorPlan

derive instance eqFloorPlanOp :: Eq FloorPlanOp