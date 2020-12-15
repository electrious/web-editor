module HouseBuilder.FloorPlanNode where

import Prelude

import Control.Alternative (empty)
import Custom.Mesh (TappableMesh)
import Data.Default (class Default, def)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_active, _id, _polygon, _tapped)
import Editor.PolygonEditor (_delete, createPolyEditor)
import FRP.Dynamic (Dynamic, latestEvt, step)
import FRP.Event (Event)
import Model.ActiveMode (ActiveMode(..), isActive)
import Model.Hardware.PanelModel (_isActive)
import Model.HouseBuilder.FloorPlan (FloorPlan, FloorPlanOp(..))
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
dragArrow :: Event Boolean -> Node FloorPlanConfig DragArrow
dragArrow actEvt = do
    mat <- view _arrowMaterial <$> getEnv
    
    let -- make sure the arrow can only be dragged between 0 and 20 in Z axis
        validator pos = let z = vecZ pos in z >= 0.0 && z <= 20.0
        -- omit the value changes in x/y axis
        transF pos    = mkVec3 0.0 0.0 (vecZ pos)
        
        cfg = def # _isActive       .~ actEvt
                  # _customMat      .~ mat
                  # _validator      .~ validator
                  # _deltaTransform .~ transF
                  
    localEnv (const (cfg :: DragObjCfg Geometry)) createDraggableObject

createFloorNode :: Node FloorPlanConfig FloorPlanNode
createFloorNode = fixNodeE \newFpEvt -> do
    cfg <- getEnv
    
    let fp  = cfg ^. _floor
        act = cfg ^. _active

        fpDyn = step fp newFpEvt
    -- render the polygon
    polyMDyn :: Dynamic TappableMesh <- localEnv (const $ cfg ^. _active) $ renderDynamic fpDyn

    -- setup the polygon editor
    editor <- createPolyEditor (isActive <$> act) (fp ^. _polygon)

    -- calculate the updated floor plan
    let updatePoly f poly = f # _polygon .~ poly
        fpEvt = updatePoly fp <$> editor ^. _polygon

        node = FloorPlanNode {
            updated : FPOUpdate <$> fpEvt,
            deleted : const (FPODelete $ fp ^. _id) <$> editor ^. _delete,
            tapped  : const fp <$> (latestEvt $ view _tapped <$> polyMDyn)
            }
    pure { input : fpEvt, output : node }
