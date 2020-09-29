module Model.Racking.FX.Bridge where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (view, (.~))
import Data.Meter (Meter, inch, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.ArrayComponent (class ArrayComponent)
import Model.Class (class HasPBUUID, class IsPBArrayComp, getArrayNumber, getUUID, getX, getY, getZ)
import Model.RoofComponent (class RoofComponent)


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
instance roofComponentBridge :: RoofComponent Bridge where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 1.0
                 # _height .~ inch 1.0
instance arrayComponentBridge :: ArrayComponent Bridge where
    arrayNumber = view _arrayNumber
instance protoDecodableBridge :: ProtoDecodable Bridge BridgePB where
    fromProto b = Bridge {
        id          : fromProto $ getUUID b,
        x           : meter $ getX b,
        y           : meter $ getY b,
        z           : meter $ getZ b,
        arrayNumber : getArrayNumber b
    }