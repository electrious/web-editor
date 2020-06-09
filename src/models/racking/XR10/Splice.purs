module Model.Racking.XR10.Splice where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Util (ffi, fpi)
import Model.Class (class HasPBUUID, class IsPBArrayComp, getArrayNumber, getUUID, getX, getY, getZ)
import Model.UUID (PBUUID)


foreign import data SplicePB :: Type
foreign import mkSplicePB :: Effect SplicePB

instance hasPBUUIDSplicePB :: HasPBUUID SplicePB
instance isPBArrayCompSplicePB :: IsPBArrayComp SplicePB

getRails :: SplicePB -> Array PBUUID
getRails = ffi ["r"] "r.getRailsList()"

setRails :: Array PBUUID -> SplicePB -> Effect Unit
setRails = fpi ["a", "r", ""] "r.setRailsList(a)"


newtype Splice = Splice {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    railIds     :: Array UUID
}

derive instance newtypeSplice :: Newtype Splice _
derive instance genericSplice :: Generic Splice _
instance showSplice :: Show Splice where
    show = genericShow
instance protoDecodableSplice :: ProtoDecodable Splice SplicePB where
    fromProto s = Splice {
        id          : fromProto $ getUUID s,
        x           : meter $ getX s,
        y           : meter $ getY s,
        z           : meter $ getZ s,
        arrayNumber : getArrayNumber s,
        railIds     : fromProto <$> getRails s
    }