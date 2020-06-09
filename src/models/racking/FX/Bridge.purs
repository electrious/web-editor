module Model.Racking.FX.Bridge where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, inch, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Class (class HasPBUUID, class IsPBArrayComp, getArrayNumber, getUUID, getX, getY, getZ)


foreign import data BridgePB :: Type
foreign import mkBridgePB :: Effect BridgePB

instance hasPBUUIdBridgePB :: HasPBUUID BridgePB
instance isPBArrayCompBridgePB :: IsPBArrayComp BridgePB

bridgeWidth :: Meter
bridgeWidth = inch 8.0

newtype Bridge = Bridge {
    id :: UUID,
    x  :: Meter,
    y  :: Meter,
    z  :: Meter,
    arrayNumber :: Int
}

derive instance newtypeBridge :: Newtype Bridge _
derive instance genericBridge :: Generic Bridge _
instance showBridge :: Show Bridge where
    show = genericShow
instance protoDecodableBridge :: ProtoDecodable Bridge BridgePB where
    fromProto b = Bridge {
        id          : fromProto $ getUUID b,
        x           : meter $ getX b,
        y           : meter $ getY b,
        z           : meter $ getZ b,
        arrayNumber : getArrayNumber b
    }