module Model.Racking.XRFlat.TiltLeg where

import Prelude

import Data.Enum (class BoundedEnum, class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Math.Angle (Angle)

data TiltLegType = South | North

derive instance eqTiltLegType :: Eq TiltLegType
derive instance ordTiltLegType :: Ord TiltLegType
derive instance genericTiltLegType :: Generic TiltLegType _
instance showTiltLegType :: Show TiltLegType where
    show = genericShow
instance boundTiltLegType :: Bounded TiltLegType where
    top = genericTop
    bottom = genericBottom
instance enumTiltLegType :: Enum TiltLegType where
    succ = genericSucc
    pred = genericPred
instance boundEnumTiltLegType :: BoundedEnum TiltLegType where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum


newtype TiltLeg = TiltLeg {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    length      :: Meter,
    tilt        :: Angle,
    arrayNumber :: Int,
    type        :: TiltLegType
}

derive instance newtypeTiltLeg :: Newtype TiltLeg _
derive instance genericTiltLeg :: Generic TiltLeg _
instance showTiltLeg :: Show TiltLeg where
    show = genericShow
