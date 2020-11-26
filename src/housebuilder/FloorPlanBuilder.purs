module HouseBuilder.FloorPlanBuilder where

import Prelude

import Control.Plus (empty)
import Data.Default (class Default)
import Data.Lens (Lens', view, (^.), (.~), (%~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (delete, insert)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_disposable, _id, _object)
import Editor.Disposable (class Disposable)
import Effect (Effect)
import FRP.Dynamic (step)
import FRP.Event (Event, create)
import Model.HouseBuilder.FloorPlan (FloorPlan, FloorPlanOp(..))
import Three.Core.Object3D (class IsObject3D, Object3D, mkObject3D, setName)

newtype FloorPlanBuilder = FloorPlanBuilder {
    object     :: Object3D,
    plans      :: Event (Array FloorPlan),
    disposable :: Effect Unit
}

derive instance newtypeFloorPlanBuilder :: Newtype FloorPlanBuilder _

instance isObject3DFloorPlanBuilder :: IsObject3D FloorPlanBuilder where
    toObject3D = view _object
instance disposableFloorPlanBuilder :: Disposable FloorPlanBuilder where
    dispose = view _disposable

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

createWrapper :: Effect Object3D
createWrapper = do
    wrapper <- mkObject3D
    setName "floor wrapper" wrapper
    pure wrapper

mkFloorPlanBuilder :: Effect FloorPlanBuilder
mkFloorPlanBuilder = do
    wrapper <- createWrapper

    { event : actFloorEvt, push : updateActive } <- create
    { event : stEvt, push : updateSt } <- create

    let actFloorDyn = step Nothing actFloorEvt


    pure $ FloorPlanBuilder {
        object     : wrapper,
        plans      : empty,
        disposable : pure unit
    }
