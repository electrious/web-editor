module Model.Racking.XR10.Clamp where

import Prelude

import Data.Enum (class BoundedEnum, class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Model.Racking.Common (RackPos)

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
