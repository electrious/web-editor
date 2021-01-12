module HouseBuilder.FloorPlanNode where

import Prelude

import Control.Alt ((<|>))
import Custom.Mesh (TapMouseMesh)
import Data.Array (foldl)
import Data.Default (class Default, def)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_active, _height, _id, _modeDyn, _mouseMove, _name, _polygon, _position, _rotation, _tapped)
import Editor.PolygonEditor (_delete, createPolyEditor)
import Editor.SceneEvent (SceneMouseMoveEvent)
import FRP.Dynamic (Dynamic, latestEvt, step)
import FRP.Event (Event, fold)
import FRP.Event.Extra (multicast)
import HouseBuilder.RoofSurfaceBuilder (editSurfaces)
import Math (pi)
import Model.ActiveMode (ActiveMode(..), isActive)
import Model.Hardware.PanelModel (_isActive)
import Model.HouseBuilder.FloorPlan (FloorPlan, FloorPlanOp(..), floorPlanTop)
import Model.Polygon (Polygon, _polyVerts)
import Rendering.DynamicNode (renderDynamic)
import Rendering.Node (Node, fixNodeE, getEnv, localEnv, node)
import Three.Core.Geometry (Geometry)
import Three.Core.Material (MeshBasicMaterial)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (Vector2, Vector3, mkVec3, vecX, vecY, vecZ)
import UI.DraggableObject (DragObjCfg, DraggableObject, _customMat, _deltaTransform, _validator, createDraggableObject)

newtype FloorPlanConfig = FloorPlanConfig {
    floor         :: FloorPlan,
    active        :: Dynamic ActiveMode,

    arrowMaterial :: Maybe MeshBasicMaterial
    }

derive instance newtypeFloorPlanConfig :: Newtype FloorPlanConfig _
instance defaultFloorPlanConfig :: Default FloorPlanConfig where
    def = FloorPlanConfig {
        floor         : def,
        active        : pure Active,
        arrowMaterial : Nothing
        }

_floor :: forall t a r. Newtype t { floor :: a | r } => Lens' t a
_floor = _Newtype <<< prop (SProxy :: SProxy "floor")

_arrowMaterial :: forall t a r. Newtype t { arrowMaterial :: a | r } => Lens' t a
_arrowMaterial = _Newtype <<< prop (SProxy :: SProxy "arrowMaterial")


newtype FloorPlanNode = FloorPlanNode {
    updated   :: Event FloorPlanOp,
    deleted   :: Event FloorPlanOp,
    tapped    :: Event FloorPlan,
    mouseMove :: Event SceneMouseMoveEvent
    }

derive instance newtypeFloorPlanNode :: Newtype FloorPlanNode _

type DragArrow = DraggableObject

-- | calculate position for drag Arrow based on all floor plan vertices
arrowPos :: FloorPlan -> Vector3
arrowPos fp = mkVec3 x y 0.5
    where vs = fp ^. _polygon <<< _polyVerts

          f Nothing v   = Just v
          f (Just ov) v = if vecX v > vecX ov then Just v else Just ov

          mv = foldl f Nothing vs

          x = maybe 2.0 (((+) 2.0) <<< vecX) mv
          y = maybe 0.0 vecY mv


-- | create the drag arrow to drag the Floor Plan to form the house
dragArrow :: Dynamic Boolean -> Dynamic Vector3 -> Node FloorPlanConfig DragArrow
dragArrow actDyn posDyn = do
    mat <- view _arrowMaterial <$> getEnv
    
    let -- make sure the arrow can only be dragged between 0 and 20 in Z axis
        validator pos = let z = vecZ pos in z >= 0.0 && z <= 20.0
        -- make sure the arrow only can be dragged along Z axis
        transF d = mkVec3 0.0 0.0 (vecZ d)
        
        cfg = def # _isActive       .~ actDyn
                  # _customMat      .~ mat
                  # _validator      .~ validator
                  # _deltaTransform .~ Just transF
                  # _rotation       .~ mkEuler (pi / 2.0) 0.0 0.0
    node (def # _position .~ posDyn) $ createDraggableObject (cfg :: DragObjCfg Geometry)


-- | setup drag arrow to edit the house height
setupHeightEditor :: Dynamic Boolean -> Dynamic Vector3 -> Node FloorPlanConfig (Event Meter)
setupHeightEditor actDyn posDyn = do
    arrow <- dragArrow actDyn posDyn
    let toH = meter <<< vecZ
    pure $ toH <$> (arrow ^. _position)


data UpdFloorOp = UpdPoly (Polygon Vector2)
                | UpdHeight Meter


applyOp :: UpdFloorOp -> FloorPlan -> FloorPlan
applyOp (UpdPoly poly) fp = fp # _polygon .~ poly
applyOp (UpdHeight h)  fp = fp # _height  .~ h

createFloorNode :: Node FloorPlanConfig FloorPlanNode
createFloorNode = do
    let opt = def # _name     .~ "floor-node"
                  # _position .~ pure (mkVec3 0.0 0.0 0.5)

    fixNodeE \newFpEvt -> node opt do
        cfg <- getEnv
    
        let fp  = cfg ^. _floor
            act = cfg ^. _active

            fpDyn = step fp newFpEvt

            isActEvt = isActive <$> act
            calcPos p = mkVec3 0.0 0.0 (meterVal $ floorPlanTop p)
        -- render the polygon
        polyMDyn :: Dynamic TapMouseMesh <- localEnv (const $ cfg ^. _active) $ renderDynamic fpDyn

        -- setup the polygon editor
        editor <- node (def # _position .~ (calcPos <$> fpDyn)) $
                      createPolyEditor $ def # _isActive .~ isActEvt
                                             # _polygon  .~ fp ^. _polygon

        -- setup the height editor
        heightEvt <- setupHeightEditor isActEvt $ arrowPos <$> fpDyn

        let conf = def # _floor     .~ fp
                       # _modeDyn   .~ pure Active
                       # _mouseMove .~ latestEvt (view _mouseMove <$> polyMDyn)
        surfsEvt <- editSurfaces conf

        -- calculate the updated floor plan
        let opEvt = (UpdPoly <$> editor ^. _polygon) <|>
                    (UpdHeight <$> heightEvt)
            fpEvt = multicast $ fold applyOp opEvt fp

            node = FloorPlanNode {
                updated   : FPOUpdate <$> fpEvt,
                deleted   : const (FPODelete $ fp ^. _id) <$> editor ^. _delete,
                tapped    : const fp <$> (latestEvt $ view _tapped <$> polyMDyn),
                mouseMove : latestEvt $ view _mouseMove <$> polyMDyn
                }
        pure { input : fpEvt, output : node }
