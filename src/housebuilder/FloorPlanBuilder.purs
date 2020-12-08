module HouseBuilder.FloorPlanBuilder where

import Prelude

import Control.Plus (empty)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (delete, insert)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_deleted, _id, _name, _tapped, _updated)
import FRP.Dynamic (Dynamic, latestEvt, step)
import FRP.Event (Event)
import FRP.Event.Extra (anyEvt, multicast)
import HouseBuilder.FloorPlanNode (FloorPlanConfig(..), FloorPlanNode, createFloorNode)
import Model.ActiveMode (ActiveMode(..))
import Model.HouseBuilder.FloorPlan (FloorPlan, FloorPlanOp(..))
import Rendering.DynamicNode (dynamic)
import Rendering.Node (Node, fixNodeEWith, node)


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


renderPlan :: forall e. Dynamic ActiveMode -> FloorPlan -> Node e FloorPlanNode
renderPlan actDyn fp = createFloorNode (FloorPlanConfig { floor : fp, active : actDyn })

-- | render all dynamic floor plans and get the tap event on any one of them
renderPlans :: forall e. Dynamic (UUIDMap FloorPlan) -> Dynamic (Maybe FloorPlan) -> Node e (Dynamic (UUIDMap FloorPlanNode))
renderPlans psDyn actP = do
    let checkMode fp Nothing  = Inactive
        checkMode fp (Just a) = if fp == a then Active else Inactive

        -- render each floor plan with active mode dynamic as local env
        doRender fp = renderPlan (checkMode fp <$> actP) fp
    dynamic $ traverse doRender <$> psDyn

getNodeEvt :: forall a. (FloorPlanNode -> Event a) -> Dynamic (UUIDMap FloorPlanNode) -> Event a
getNodeEvt f = multicast <<< latestEvt <<< map (anyEvt <<< map f)

-- | create FloorPlan builder node and setup all events necessary.
buildFloorPlan :: forall e. Node e (Event (Array FloorPlan))
buildFloorPlan = node (def # _name .~ "floor plan builder") $
    fixNodeEWith Nothing \actFloorEvt ->
        fixNodeEWith (def :: FloorPlanState) \stEvt -> do
            let plansEvt    = compact $ view _floorsToRender <$> stEvt
                actFloorDyn = step Nothing actFloorEvt
                plansDyn    = step M.empty plansEvt

            -- render the floor plans
            nodesMapDyn <- renderPlans plansDyn actFloorDyn
            
            -- get the tapped floor plan and set it active
            let planTappedEvt = Just <$> getNodeEvt (view _tapped) nodesMapDyn
                planUpdEvt = getNodeEvt (view _updated) nodesMapDyn
                planDelEvt = getNodeEvt (view _deleted) nodesMapDyn

            pure { input : empty, output : { input : planTappedEvt, output : empty } }
