module Model.Racking.XR10.Clamp where

import Prelude

import Data.Default (def)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Show.Generic (genericShow)
import Data.Lens (view, (.~))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.Racking.Common (RackPos)
import Model.RoofComponent (class RoofComponent)

data ClampType = Middle | End

derive instance eqClampType :: Eq ClampType
derive instance ordClampType :: Ord ClampType
derive instance genericClampType :: Generic ClampType _
instance showClampType :: Show ClampType where
    show = genericShow
instance boundClampType :: Bounded ClampType where
    top = genericTop
    bottom = genericBottom
instance enumClampType :: Enum ClampType where
    succ = genericSucc
    pred = genericPred
instance boundEnumClampType :: BoundedEnum ClampType where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum

newtype Clamp = Clamp {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    clampType   :: ClampType,
    clampPos    :: RackPos
}

derive instance newtypeClamp :: Newtype Clamp _
derive instance genericClamp :: Generic Clamp _
instance showClamp :: Show Clamp where
    show = genericShow
instance roofComponentClamp :: RoofComponent Clamp where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ meter 0.03
                 # _height .~ meter 0.03
instance arrayComponentClamp :: ArrayComponent Clamp where
    arrayNumber = view _arrayNumber
