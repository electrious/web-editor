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
import Editor.HouseBuilder.GraphEditor (VertMerger(..), _graph, _heightEditable, _lineCenter, _vertMerger, createGraphEditor)
import Editor.PanelAPIInterpreter (_finished)
import Editor.PolygonEditor (_delete, _showFinish, createPolyEditor)
import Editor.SceneEvent (SceneMouseMoveEvent)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, current, dynEvent, latestEvt, performDynamic, step)
import FRP.Event (Event, fold)
import FRP.Event.Extra (multicast)
import Math.Line (Line, _end, _start, lineCenter)
import Model.ActiveMode (ActiveMode(..), isActive)
import Model.HouseBuilder.FloorPlan (FloorPlan, FloorPlanOp(..), floorGraph, floorPlanTop)
import Model.HouseBuilder.HousePoint (HousePoint, HousePointType(..), _pointType, mergeHousePoint)
import Model.Polygon (Polygon, _polyVerts)
import Model.UUID (assignNewId)
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

-- calculate center point for a house line, make the center a RidgePoint
-- if at least one end of the line is a RidgePoint
houseLineCenter :: Line HousePoint -> Effect HousePoint
houseLineCenter l = do
    c <- assignNewId $ lineCenter l
    let s = l ^. _start
        e = l ^. _end
        
        f RidgePoint _ = RidgePoint
        f _ RidgePoint = RidgePoint
        f _ _          = GutterPoint
    pure $ c # _pointType .~ f (s ^. _pointType) (e ^. _pointType)


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
        editor <- node (def # _position .~ topPosDyn) $
                      fixNodeE \finishedEvt -> do
                          -- graph editor for the roof top
                          let gDyn :: Dynamic (Graph HousePoint Int)
                              gDyn = performDynamic $ floorGraph <$> fpDyn
                          roofTopEvt <- createGraphEditor $ def # _active         .~ step Inactive (const Active <$> finishedEvt)
                                                                # _floor          .~ fpDyn
                                                                # _graph          .~ gDyn
                                                                # _mouseMove      .~ latestEvt (view _mouseMove <$> polyMDyn)
                                                                # _vertMerger     .~ VertMerger mergeHousePoint
                                                                # _heightEditable .~ ((==) RidgePoint <<< view _pointType)
                                                                # _lineCenter     .~ houseLineCenter

                          defAct <- liftEffect $ current act
                          editor <- createPolyEditor $ def # _active     .~ step defAct (dynEvent act <|> (const Inactive <$> finishedEvt))
                                                           # _polygon    .~ fp ^. _polygon
                                                           # _showFinish .~ true
                          pure { input : editor ^. _finished, output : editor }


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
