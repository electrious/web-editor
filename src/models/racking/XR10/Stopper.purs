module Model.Racking.XR10.Stopper where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Racking.Common (RackPos)
import Model.Class (class HasPBUUID, class HasPos, class IsPBArrayComp, getArrayNumber, getPos, getUUID, getX, getY, getZ)

foreign import data StopperPB :: Type
foreign import mkStopperPB :: Effect StopperPB

instance hasPBUUIDStopperPB :: HasPBUUID StopperPB
instance isPBArrayCompStopperPB :: IsPBArrayComp StopperPB
instance hasPosStopperPB :: HasPos StopperPB

newtype Stopper = Stopper {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    type        :: RackPos
}

derive instance newtypeStopper :: Newtype Stopper _
derive instance genericStopper :: Generic Stopper _
instance showStopper :: Show Stopper where
    show = genericShow
instance protoDecodableStopper :: ProtoDecodable Stopper StopperPB where
    fromProto s = Stopper {
        id          : fromProto $ getUUID s,
        x           : meter $ getX s,
        y           : meter $ getY s,
        z           : meter $ getZ s,
        arrayNumber : getArrayNumber s,
        type        : fromProto $ getPos s
    }