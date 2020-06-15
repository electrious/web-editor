module Model.Racking.BX.Block where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (view, (.~))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.ArrayComponent (class ArrayComponent)
import Model.Class (class HasPBUUID, class IsPBArrayComp, getArrayNumber, getUUID, getX, getY, getZ)
import Model.RoofComponent (class RoofComponent)

foreign import data BlockPB :: Type
foreign import mkBlockPB :: Effect BlockPB

instance hasPBUUIDBlockPB :: HasPBUUID BlockPB
instance isPBArrayCompBlockPB :: IsPBArrayComp BlockPB

newtype Block = Block {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int
}

derive instance newtypeBlock :: Newtype Block _
derive instance genericBlock :: Generic Block _
instance showBlock :: Show Block where
    show = genericShow
instance roofComponentBlock :: RoofComponent Block where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ meter 0.2
                 # _height .~ meter 0.2
instance arrayComponentBlock :: ArrayComponent Block where
    arrayNumber = view _arrayNumber
instance protoDecodableBlock :: ProtoDecodable Block BlockPB where
    fromProto b = Block {
        id          : fromProto $ getUUID b,
        x           : meter $ getX b,
        y           : meter $ getY b,
        z           : meter $ getZ b,
        arrayNumber : getArrayNumber b
    }