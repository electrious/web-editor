module HouseBuilder.FloorPlanBuilder where

import Prelude

import Data.Lens (view)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_disposable, _object)
import Editor.Disposable (class Disposable)
import Effect (Effect)
import FRP.Event (Event)
import Model.HouseBuilder.FloorPlan (FloorPlan)
import Three.Core.Object3D (class IsObject3D, Object3D)

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

