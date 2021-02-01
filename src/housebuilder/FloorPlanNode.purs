module HouseBuilder.FloorPlanNode where

import Prelude

import Control.Alt ((<|>))
import Custom.Mesh (TapMouseMesh)
import Data.Default (class Default, def)
import Data.Graph (Graph)
import Data.Lens (view, (.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meterVal)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_active, _floor, _height, _id, _mouseMove, _name, _polygon, _position, _tapped)
import Editor.HeightEditor (_arrowMaterial, dragArrowPos, mkDragArrowConf, setupHeightEditor)
import Editor.HouseBuilder.GraphEditor (VertMerger(..), _graph, _heightEditable, _vertMerger, createGraphEditor)
import Editor.PolygonEditor (_delete, createPolyEditor)
import Editor.SceneEvent (SceneMouseMoveEvent)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, latestEvt, step)
import FRP.Event (Event, fold)
import FRP.Event.Extra (multicast)
import Model.ActiveMode (ActiveMode(..), isActive)
import Model.HouseBuilder.FloorPlan (FloorPlan, FloorPlanOp(..), floorGraph, floorPlanTop)
import Model.HouseBuilder.HousePoint (HousePoint, HousePointType(..), _pointType, mergeHousePoint)
import Model.Polygon (Polygon, _polyVerts)
import Rendering.DynamicNode (renderDynamic)
import Rendering.Node (Node, fixNodeE, getEnv, localEnv, node)
import Three.Core.Material (MeshBasicMaterial)
import Three.Math.Vector (Vector2, Vector3, mkVec3)

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


newtype FloorPlanNode = FloorPlanNode {
    updated   :: Event FloorPlanOp,
    deleted   :: Event FloorPlanOp,
    tapped    :: Event FloorPlan,
    mouseMove :: Event SceneMouseMoveEvent
    }

derive instance newtypeFloorPlanNode :: Newtype FloorPlanNode _

-- | calculate position for drag Arrow based on all floor plan vertices
arrowPos :: FloorPlan -> Vector3
arrowPos fp = dragArrowPos (fp ^. _polygon <<< _polyVerts)

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

            isActDyn = isActive <$> act
            calcPos p = mkVec3 0.0 0.0 (meterVal $ floorPlanTop p)

            topPosDyn = calcPos <$> fpDyn
        -- render the polygon
        polyMDyn :: Dynamic TapMouseMesh <- localEnv (const $ cfg ^. _active) $ renderDynamic fpDyn

        -- setup the polygon editor
        editor <- node (def # _position .~ topPosDyn) do
                      -- graph editor for the roof top
                      g :: Graph HousePoint Int <- liftEffect $ floorGraph fp
                      roofTopEvt <- createGraphEditor $ def # _active         .~ pure Inactive
                                                            # _floor          .~ fpDyn
                                                            # _graph          .~ g
                                                            # _mouseMove      .~ latestEvt (view _mouseMove <$> polyMDyn)
                                                            # _vertMerger     .~ VertMerger mergeHousePoint
                                                            # _heightEditable .~ ((==) RidgePoint <<< view _pointType)
            
                      createPolyEditor $ def # _active  .~ act
                                             # _polygon .~ fp ^. _polygon


        -- setup the height editor
        heightEvt <- localEnv (view _arrowMaterial >>> mkDragArrowConf) $ setupHeightEditor act $ arrowPos <$> fpDyn


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
