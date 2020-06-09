module Model.Racking.XR10.Rail where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, feetInch, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Util (ffi, fpi)
import Model.Class (class HasLength, class HasPBUUID, class IsPBArrayComp, getArrayNumber, getLength, getUUID, getX, getY, getZ)
import Model.UUID (PBUUID)

foreign import data RailPB :: Type
foreign import mkRailPB :: Effect RailPB

instance hasPBUUIDRailPB :: HasPBUUID RailPB
instance isPBArrayCompRailPB :: IsPBArrayComp RailPB
instance hasLengthRailPB :: HasLength RailPB

getPanels :: RailPB -> Array PBUUID
getPanels = ffi ["r"] "r.getPanelsList()"

setPanels :: Array PBUUID -> RailPB -> Effect Unit
setPanels = fpi ["a", "r", ""] "r.setPanelsList(a)"


railLength :: Meter
railLength = feetInch 14.0 0.0

newtype Rail = Rail {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter,
    panelIds    :: Array UUID
}

derive instance newtypeRail :: Newtype Rail _
derive instance genericRail :: Generic Rail _
instance showRail :: Show Rail where
    show = genericShow
instance protoDecodableRail :: ProtoDecodable Rail RailPB where
    fromProto r = Rail {
        id          : fromProto $ getUUID r,
        x           : meter $ getX r,
        y           : meter $ getY r,
        z           : meter $ getZ r,
        arrayNumber : getArrayNumber r,
        length      : meter $ getLength r,
        panelIds    : fromProto <$> getPanels r
    }