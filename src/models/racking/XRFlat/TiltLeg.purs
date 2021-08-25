module Model.Racking.XRFlat.TiltLeg where

import Prelude hiding (degree)

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Default (def)
import Data.Either (note)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Lens (view, (.~))
import Data.Meter (Meter, inch)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Math.Angle (Angle)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)

data TiltLegType = South | North

derive instance Eq TiltLegType
derive instance Ord TiltLegType
derive instance Generic TiltLegType _
instance Show TiltLegType where
    show = genericShow
instance Bounded TiltLegType where
    top = genericTop
    bottom = genericBottom
instance Enum TiltLegType where
    succ = genericSucc
    pred = genericPred
instance BoundedEnum TiltLegType where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance EncodeJson TiltLegType where
    encodeJson = fromEnum >>> encodeJson
instance DecodeJson TiltLegType where
    decodeJson = decodeJson >=> toEnum >>> note (TypeMismatch "Invalid value for TiltLegType")

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

derive instance Newtype TiltLeg _
derive instance Generic TiltLeg _
instance Show TiltLeg where
    show = genericShow
instance RoofComponent TiltLeg where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 1.0
                 # _height .~ inch 1.0
instance ArrayComponent TiltLeg where
    arrayNumber = view _arrayNumber
instance EncodeJson TiltLeg where
    encodeJson (TiltLeg t) = "id"   := t.id
                          ~> "an"   := t.arrayNumber
                          ~> "x"    := t.x
                          ~> "y"    := t.y
                          ~> "z"    := t.z
                          ~> "l"    := t.length
                          ~> "tilt" := t.tilt
                          ~> "t"    := t.type
                          ~> jsonEmptyObject
instance DecodeJson TiltLeg where
    decodeJson = decodeJson >=> f
        where f o = mkTiltLeg <$> o .: "id"
                              <*> o .: "an"
                              <*> o .: "x"
                              <*> o .: "y"
                              <*> o .: "z"
                              <*> o .: "l"
                              <*> o .: "tilt"
                              <*> o .: "t"

mkTiltLeg :: UUID -> Int -> Meter -> Meter -> Meter -> Meter -> Angle -> TiltLegType -> TiltLeg
mkTiltLeg id arrayNumber x y z length tilt t = TiltLeg { id: id, x: x, y: y, z: z, length: length, tilt: tilt, arrayNumber: arrayNumber, type: t }