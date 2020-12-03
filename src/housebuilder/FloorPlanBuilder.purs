module HouseBuilder.FloorPlanBuilder where

import Prelude

import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (delete, insert)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_id, _name)
import Effect.Class (liftEffect)
import FRP.Dynamic (step)
import FRP.Event (Event, create)
import Model.HouseBuilder.FloorPlan (FloorPlan, FloorPlanOp(..))
import Rendering.Node (Node, node)


-- internal data structure to manage floor plans
newtype FloorPlanState = FloorPlanState {
    floors         :: UUIDMap FloorPlan,
    floorsToRender :: Maybe (UUIDMap FloorPlan)
}

derive instance newtypeFloorPlanState :: Newtype FloorPlanState _
instance defaultFloorPlanState :: Default FloorPlanState where
    def = FloorPlanState {
        floors         : M.empty,
        floorsToRender : Nothing
    }

_floors :: forall t a r. Newtype t { floors :: a | r } => Lens' t a
_floors = _Newtype <<< prop (SProxy :: SProxy "floors")

_floorsToRender :: forall t a r. Newtype t { floorsToRender :: a | r } => Lens' t a
_floorsToRender = _Newtype <<< prop (SProxy :: SProxy "floorsToRender")

-- mark all floors to be rendered
renderAll :: FloorPlanState -> FloorPlanState
renderAll s = s # _floorsToRender .~ Just (s ^. _floors)

-- | update the managed floors with new operation
applyFloorOp :: FloorPlanOp -> FloorPlanState -> FloorPlanState
applyFloorOp (FPOCreate fp)  s = renderAll $ s # _floors %~ insert (fp ^. _id) fp
applyFloorOp (FPODelete fid) s = renderAll $ s # _floors %~ delete fid
applyFloorOp (FPOUpdate fp)  s = s # _floors %~ insert (fp ^. _id) fp
                                   # _floorsToRender .~ Nothing


-- | create FloorPlan builder node and setup all events necessary.
buildFloorPlan :: forall e. Node e (Event (Array FloorPlan))
buildFloorPlan = node (def # _name .~ "floor plan builder") do
    { event : actFloorEvt, push : updateActive } <- liftEffect create
    { event : stEvt, push : updateSt } <- liftEffect create

    let actFloorDyn = step Nothing actFloorEvt
        
    pure empty
