module HouseBuilder.FloorPlanBuilder where

import Prelude

import Control.Monad.Writer (tell)
import Control.Plus (empty)
import Custom.Mesh (TappableMesh)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (delete, insert, lookup)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.UUID (UUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_id, _name, _tapped)
import Editor.Disposable (Disposee(..))
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, latestEvt, sampleDyn, step)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Extra (anyEvt)
import Model.ActiveMode (ActiveMode(..))
import Model.HouseBuilder.FloorPlan (FloorPlan, FloorPlanOp(..))
import Rendering.DynamicNode (dynamic)
import Rendering.Node (Node, localEnv, node)
import Rendering.NodeRenderable (render)


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

-- | render all dynamic floor plans and get the tap event on any one of them
renderPlans :: forall e. Dynamic (UUIDMap FloorPlan) -> Dynamic (Maybe FloorPlan) -> Node e (Event UUID)
renderPlans psDyn actP = do
    let checkMode fp Nothing  = Inactive
        checkMode fp (Just a) = if fp == a then Active else Inactive

        -- render each floor plan with active mode dynamic as local env
        doRender fp = localEnv (const $ checkMode fp <$> actP) (render fp)
    meshMapDyn :: Dynamic (UUIDMap TappableMesh) <- dynamic $ traverse doRender <$> psDyn

    -- get tap events from all rendered plans
    let tapPlan i m = const i <$> m ^. _tapped
        tapEvt = latestEvt $ (anyEvt <<< mapWithIndex tapPlan) <$> meshMapDyn
    pure tapEvt



-- | create FloorPlan builder node and setup all events necessary.
buildFloorPlan :: forall e. Node e (Event (Array FloorPlan))
buildFloorPlan = node (def # _name .~ "floor plan builder") do
    -- create active floor and floor plan state events
    { event : actFloorEvt, push : updateActive } <- liftEffect create
    { event : stEvt, push : updateSt } <- liftEffect create

    let plansEvt    = compact $ view _floorsToRender <$> stEvt
        actFloorDyn = step Nothing actFloorEvt
        plansDyn    = step M.empty plansEvt

    -- render the floor plans
    planTapEvt <- renderPlans plansDyn actFloorDyn

    -- get the tapped floor plan and set it active
    let planTappedEvt = sampleDyn plansDyn (lookup <$> planTapEvt)
    d1 <- liftEffect $ subscribe planTappedEvt updateActive
    tell $ Disposee d1

    -- set the default state
    liftEffect $ updateSt (def :: FloorPlanState)


    pure empty
