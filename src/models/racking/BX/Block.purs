module Model.Racking.BX.Block where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Class (class HasPBUUID, class IsPBArrayComp, getArrayNumber, getUUID, getX, getY, getZ)

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
instance protoDecodableBlock :: ProtoDecodable Block BlockPB where
    fromProto b = Block {
        id          : fromProto $ getUUID b,
        x           : meter $ getX b,
        y           : meter $ getY b,
        z           : meter $ getZ b,
        arrayNumber : getArrayNumber b
    }