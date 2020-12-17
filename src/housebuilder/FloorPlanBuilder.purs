module HouseBuilder.FloorPlanBuilder where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (delete, insert)
import Data.Map as M
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_active, _deleted, _face, _id, _mouseMove, _name, _parent, _point, _position, _tapped, _updated)
import Editor.PolygonAdder (_addedPoint, createPolygonAdder, mkCandidatePoint)
import Editor.SceneEvent (SceneMouseMoveEvent)
import FRP.Dynamic (Dynamic, gateDyn, latestEvt, step)
import FRP.Event (Event, fold)
import FRP.Event.Extra (anyEvt, multicast, performEvent)
import HouseBuilder.FloorPlanNode (FloorPlanNode, _floor, createFloorNode)
import Model.ActiveMode (ActiveMode(..))
import Model.HouseBuilder.FloorPlan (FloorPlan, FloorPlanOp(..), newFloorPlan)
import Rendering.DynamicNode (dynamic)
import Rendering.Node (Node, _env, fixNodeEWith, localEnv, node)
import Three.Core.Face3 (normal)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (toVec2)

newtype FloorPlanBuilderConf = FloorPlanBuilderConf {
    mouseMove :: Event SceneMouseMoveEvent,
    canEdit   :: Dynamic Boolean
    }

derive instance newtypeFloorPlanBuilderConf :: Newtype FloorPlanBuilderConf _
instance defaultFloorPlanBuilderConf :: Default FloorPlanBuilderConf where
    def = FloorPlanBuilderConf {
        mouseMove : empty,
        canEdit   : step false empty
        }

_canEdit :: forall t a r. Newtype t { canEdit :: a | r } => Lens' t a
_canEdit = _Newtype <<< prop (SProxy :: SProxy "canEdit")

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
renderPlan actDyn fp = localEnv (const $ def # _floor  .~ fp
                                             # _active .~ actDyn) createFloorNode

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

-- | setup PolygonAdder to add new FloorPlan
setupFloorAdder :: Dynamic (Maybe FloorPlan) -> Node FloorPlanBuilderConf (Event FloorPlan)
setupFloorAdder actFloorDyn = do
    e <- ask
    let parent = e ^. _parent
        conf   = e ^. _env
    let canShowAdder = (&&) <$> (isNothing <$> actFloorDyn) <*> (conf ^. _canEdit)

        -- get a candidate point
        getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            pure $ Just $ mkCandidatePoint np (normal $ evt ^. _face)

        mouseEvt   = conf ^. _mouseMove
        -- candidate point dynamic
        candPntDyn = step Nothing $ performEvent $ getCandPoint <$> gateDyn canShowAdder mouseEvt
        -- add PolygonAdder
    adder <- createPolygonAdder candPntDyn canShowAdder
    pure $ performEvent $ newFloorPlan <<< toVec2 <<< view _position <$> (adder ^. _addedPoint)


-- | create FloorPlan builder node and setup all events necessary.
buildFloorPlan :: Node FloorPlanBuilderConf (Event (UUIDMap FloorPlan))
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
                planUpdEvt    = getNodeEvt (view _updated) nodesMapDyn
                planDelEvt    = getNodeEvt (view _deleted) nodesMapDyn

            planAddEvt <- setupFloorAdder actFloorDyn
            -- combine all floor plan operations
            let opEvt    = (FPOCreate <$> planAddEvt) <|> planDelEvt <|> planUpdEvt
                -- apply operations to update the internal state
                newStEvt = fold applyFloorOp opEvt def

                -- final edited floor plans
                floorsEvt = view _floors <$> newStEvt

                newActFloorEvt = (Just <$> planAddEvt) <|> planTappedEvt
            
            pure { input : newStEvt, output : { input : newActFloorEvt, output : floorsEvt } }
