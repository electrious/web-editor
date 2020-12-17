module HouseBuilder.FloorPlanNode where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (empty)
import Custom.Mesh (TappableMesh)
import Data.Default (class Default, def)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_active, _height, _id, _polygon, _position, _tapped)
import Editor.PolygonEditor (_delete, createPolyEditor)
import FRP.Dynamic (Dynamic, latestEvt, step)
import FRP.Event (Event, fold)
import FRP.Event.Extra (multicast)
import Model.ActiveMode (ActiveMode(..), isActive)
import Model.Hardware.PanelModel (_isActive)
import Model.HouseBuilder.FloorPlan (FloorPlan, FloorPlanOp(..))
import Model.Polygon (Polygon)
import Rendering.DynamicNode (renderDynamic)
import Rendering.Node (Node, fixNodeE, getEnv, localEnv)
import Three.Core.Geometry (Geometry)
import Three.Core.Material (MeshBasicMaterial)
import Three.Math.Vector (mkVec3, vecZ)
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
        active        : step Active empty,
        arrowMaterial : Nothing
        }

_floor :: forall t a r. Newtype t { floor :: a | r } => Lens' t a
_floor = _Newtype <<< prop (SProxy :: SProxy "floor")

_arrowMaterial :: forall t a r. Newtype t { arrowMaterial :: a | r } => Lens' t a
_arrowMaterial = _Newtype <<< prop (SProxy :: SProxy "arrowMaterial")


newtype FloorPlanNode = FloorPlanNode {
    updated :: Event FloorPlanOp,
    deleted :: Event FloorPlanOp,
    tapped  :: Event FloorPlan
    }

derive instance newtypeFloorPlanNode :: Newtype FloorPlanNode _

type DragArrow = DraggableObject

-- | create the drag arrow to drag the Floor Plan to form the house
dragArrow :: Dynamic Boolean -> Node FloorPlanConfig DragArrow
dragArrow actDyn = do
    mat <- view _arrowMaterial <$> getEnv
    
    let -- make sure the arrow can only be dragged between 0 and 20 in Z axis
        validator pos = let z = vecZ pos in z >= 0.0 && z <= 20.0
        -- omit the value changes in x/y axis
        transF pos    = mkVec3 0.0 0.0 (vecZ pos)
        
        cfg = def # _isActive       .~ actDyn
                  # _customMat      .~ mat
                  # _validator      .~ validator
                  # _deltaTransform .~ transF
                  
    localEnv (const (cfg :: DragObjCfg Geometry)) createDraggableObject


-- | setup drag arrow to edit the house height
setupHeightEditor :: Dynamic Boolean -> Node FloorPlanConfig (Event Meter)
setupHeightEditor actDyn = do
    arrow <- dragArrow actDyn
    let toH = meter <<< vecZ
    pure $ toH <$> (arrow ^. _position)


data UpdFloorOp = UpdPoly Polygon
                | UpdHeight Meter


applyOp :: UpdFloorOp -> FloorPlan -> FloorPlan
applyOp (UpdPoly poly) fp = fp # _polygon .~ poly
applyOp (UpdHeight h)  fp = fp # _height  .~ h

createFloorNode :: Node FloorPlanConfig FloorPlanNode
createFloorNode = fixNodeE \newFpEvt -> do
    cfg <- getEnv
    
    let fp  = cfg ^. _floor
        act = cfg ^. _active

        fpDyn = step fp newFpEvt

        isActEvt = isActive <$> act
    -- render the polygon
    polyMDyn :: Dynamic TappableMesh <- localEnv (const $ cfg ^. _active) $ renderDynamic fpDyn

    -- setup the polygon editor
    editor <- createPolyEditor isActEvt (fp ^. _polygon)

    -- setup the height editor
    --heightEvt <- setupHeightEditor isActEvt
    let heightEvt = empty

    -- calculate the updated floor plan
    let opEvt = (UpdPoly <$> editor ^. _polygon) <|>
                (UpdHeight <$> heightEvt)
        fpEvt = multicast $ fold applyOp opEvt fp

        node = FloorPlanNode {
            updated : FPOUpdate <$> fpEvt,
            deleted : const (FPODelete $ fp ^. _id) <$> editor ^. _delete,
            tapped  : const fp <$> (latestEvt $ view _tapped <$> polyMDyn)
            }
    pure { input : fpEvt, output : node }
