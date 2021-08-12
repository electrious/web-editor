module Model.Racking.XRFlat.TiltLeg where

import Prelude hiding (degree)

import Data.Default (def)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Show.Generic (genericShow)
import Data.Lens (view, (.~))
import Data.Meter (Meter, inch)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Math.Angle (Angle)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)

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
instance roofComponentTiltLeg :: RoofComponent TiltLeg where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 1.0
                 # _height .~ inch 1.0
instance arrayComponentTiltLeg :: ArrayComponent TiltLeg where
    arrayNumber = view _arrayNumber
