module Model.Racking.XR10.LFoot where

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

foreign import data LFootPB :: Type
foreign import mkLFootPB :: Effect LFootPB

instance hasPBUUIDLFootPB :: HasPBUUID LFootPB
instance isPBArrayCompLFootPB :: IsPBArrayComp LFootPB

getFlashId :: LFootPB -> PBUUID
getFlashId = ffi ["r"] "r.getFlash()"

setFlashId :: PBUUID -> LFootPB -> Effect Unit
setFlashId = fpi ["u", "r", ""] "r.setFlash(u)"

newtype LFoot = LFoot {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    flashId     :: UUID
}

derive instance newtypeLFoot :: Newtype LFoot _
derive instance genericLFoot :: Generic LFoot _
instance showLFoot :: Show LFoot where
    show = genericShow
instance roofComponentLFoot :: RoofComponent LFoot where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 1.0
                 # _height .~ inch 1.0
instance arrayComponentLFoot :: ArrayComponent LFoot where
    arrayNumber = view _arrayNumber
instance protoDecodableLFoot :: ProtoDecodable LFoot LFootPB where
    fromProto l = LFoot {
        id          : fromProto $ getUUID l,
        x           : meter $ getX l,
        y           : meter $ getY l,
        z           : meter $ getZ l,
        arrayNumber : getArrayNumber l,
        flashId     : fromProto $ getFlashId l
    }