module Model.Racking.XR10.Clamp where

import Prelude

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
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.Racking.Common (RackPos)
import Model.RoofComponent (class RoofComponent)

data ClampType = Middle | End

derive instance Eq ClampType
derive instance Ord ClampType
derive instance Generic ClampType _
instance Show ClampType where
    show = genericShow
instance Bounded ClampType where
    top = genericTop
    bottom = genericBottom
instance Enum ClampType where
    succ = genericSucc
    pred = genericPred
instance BoundedEnum ClampType where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance EncodeJson ClampType where
    encodeJson = fromEnum >>> encodeJson
instance DecodeJson ClampType where
    decodeJson = decodeJson >=> toEnum >>> note (TypeMismatch "Invalid value for ClampType")

newtype Clamp = Clamp {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    clampType   :: ClampType,
    clampPos    :: RackPos
}

derive instance Newtype Clamp _
derive instance Generic Clamp _
instance Show Clamp where
    show = genericShow
instance RoofComponent Clamp where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ meter 0.03
                 # _height .~ meter 0.03
instance ArrayComponent Clamp where
    arrayNumber = view _arrayNumber
instance EncodeJson Clamp where
    encodeJson (Clamp c) = "id"  := c.id
                        ~> "an"  := c.arrayNumber
                        ~> "x"   := c.x
                        ~> "y"   := c.y
                        ~> "z"   := c.z
                        ~> "t"   := c.clampType
                        ~> "pos" := c.clampPos
                        ~> jsonEmptyObject
instance DecodeJson Clamp where
    decodeJson = decodeJson >=> f
        where f o = mkClamp <$> o .: "id"
                            <*> o .: "an"
                            <*> o .: "t"
                            <*> o .: "x"
                            <*> o .: "y"
                            <*> o .: "z"
                            <*> o .: "pos"

mkClamp :: UUID -> Int -> ClampType -> Meter -> Meter -> Meter -> RackPos -> Clamp
mkClamp id arrayNumber clampType x y z clampPos = Clamp { id: id, x: x, y: y, z: z, arrayNumber: arrayNumber, clampType: clampType, clampPos: clampPos }