module Model.Racking.XR10.Splice where

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
import Model.UUID (PBUUID)
import Util (ffi, fpi)


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
instance roofComponentSplice :: RoofComponent Splice where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 7.0
                 # _height .~ inch 1.0
instance arrayComponentSplice :: ArrayComponent Splice where
    arrayNumber = view _arrayNumber
instance protoDecodableSplice :: ProtoDecodable Splice SplicePB where
    fromProto s = Splice {
        id          : fromProto $ getUUID s,
        x           : meter $ getX s,
        y           : meter $ getY s,
        z           : meter $ getZ s,
        arrayNumber : getArrayNumber s,
        railIds     : fromProto <$> getRails s
    }