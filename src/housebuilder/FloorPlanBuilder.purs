module HouseBuilder.FloorPlanBuilder where

import Data.Lens (view)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_object)
import Three.Core.Object3D (class IsObject3D, Object3D)

newtype FloorPlanBuilder = FloorPlanBuilder {
    object :: Object3D
}

derive instance newtypeFloorPlanBuilder :: Newtype FloorPlanBuilder _

instance isObject3DFloorPlanBuilder :: IsObject3D FloorPlanBuilder where
    toObject3D = view _object
