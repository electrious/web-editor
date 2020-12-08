module HouseBuilder.FloorPlanNode where

import Prelude

import Custom.Mesh (TappableMesh)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_active, _id, _polygon, _tapped)
import Editor.PolygonEditor (_delete, createPolyEditor)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Model.ActiveMode (ActiveMode, isActive)
import Model.HouseBuilder.FloorPlan (FloorPlan, FloorPlanOp(..))
import Rendering.Node (Node, localEnv)
import Rendering.NodeRenderable (render)

newtype FloorPlanConfig = FloorPlanConfig {
    floor  :: FloorPlan,
    active :: Dynamic ActiveMode
    }

derive instance newtypeFloorPlanConfig :: Newtype FloorPlanConfig _

newtype FloorPlanNode = FloorPlanNode {
    floor   :: FloorPlan,

    updated :: Event FloorPlanOp,
    deleted :: Event FloorPlanOp,
    tapped  :: Event FloorPlan
    }

derive instance newtypeFloorPlanNode :: Newtype FloorPlanNode _

_floor :: forall t a r. Newtype t { floor :: a | r } => Lens' t a
_floor = _Newtype <<< prop (SProxy :: SProxy "floor")


createFloorNode :: forall e. FloorPlanConfig -> Node e FloorPlanNode
createFloorNode cfg = do
    let fp  = cfg ^. _floor
        act = cfg ^. _active
    -- render the polygon
    polyM :: TappableMesh <- localEnv (const $ cfg ^. _active) $ render (cfg ^. _floor)

    -- setup the polygon editor
    editor <- createPolyEditor (isActive <$> act) (fp ^. _polygon)

    -- calculate the updated floor plan
    let updatePoly f poly = f # _polygon .~ poly
        fpEvt = updatePoly fp <$> editor ^. _polygon

    pure $ FloorPlanNode {
        floor   : fp,
        updated : FPOUpdate <$> fpEvt,
        deleted : const (FPODelete $ fp ^. _id) <$> editor ^. _delete,
        tapped  : const fp <$> polyM ^. _tapped
        }
