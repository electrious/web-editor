module Model.Racking.FX.Skirt where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (view, (^.), (.~))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _length, _width, _x, _y, _z)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.ArrayComponent (class ArrayComponent)
import Model.Class (class HasLength, class HasPBUUID, class IsPBArrayComp, getArrayNumber, getLength, getUUID, getX, getY, getZ)
import Model.RoofComponent (class RoofComponent)


foreign import data SkirtPB :: Type
foreign import mkSkirtPB :: Effect SkirtPB

instance hasPBUUIDSkirtPB :: HasPBUUID SkirtPB
instance isPBArrayCompSkirtPB :: IsPBArrayComp SkirtPB
instance hasLengthSkirtPB :: HasLength SkirtPB

newtype Skirt = Skirt {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter
}

derive instance newtypeSkirt :: Newtype Skirt _
derive instance genericSKirt :: Generic Skirt _
instance showSkirt :: Show Skirt where
    show = genericShow
instance roofComponentSkirt :: RoofComponent Skirt where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size s = def # _width  .~ s ^. _length
                 # _height .~ meter 0.03
instance arrayComponentSkirt :: ArrayComponent Skirt where
    arrayNumber = view _arrayNumber
instance protoDecodableSkirt :: ProtoDecodable Skirt SkirtPB where
    fromProto s = Skirt {
        id          : fromProto $ getUUID s,
        x           : meter $ getX s,
        y           : meter $ getY s,
        z           : meter $ getZ s,
        arrayNumber : getArrayNumber s,
        length      : meter $ getLength s
    }