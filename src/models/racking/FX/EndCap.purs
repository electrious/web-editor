module Model.Racking.FX.EndCap where

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
import Model.Class (class HasPBUUID, class HasPos, class IsPBArrayComp, getArrayNumber, getPos, getUUID, getX, getY, getZ)
import Model.Racking.Common (RackPos)
import Model.RoofComponent (class RoofComponent)
import Model.UUID (PBUUID)
import Util (ffi, fpi)

foreign import data EndCapPB :: Type
foreign import mkEndCapPB :: Effect EndCapPB

instance hasPBUUIdEndCapPB :: HasPBUUID EndCapPB
instance isPBArrayCompEndCapPB :: IsPBArrayComp EndCapPB
instance hasPosEndCapPB :: HasPos EndCapPB

getSkirt :: EndCapPB -> PBUUID
getSkirt = ffi ["e"] "e.getSkirt()"

setSkirt :: PBUUID -> EndCapPB -> Effect Unit
setSkirt = fpi ["s", "e", ""] "e.setSkirt(s)"

newtype EndCap = EndCap {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    skirtId     :: UUID,
    position    :: RackPos
}

derive instance newtypeEndCap :: Newtype EndCap _
derive instance genericEndCap :: Generic EndCap _
instance showEndCap :: Show EndCap where
    show = genericShow
instance roofComponentEndCap :: RoofComponent EndCap where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ meter 0.001
                 # _height .~ meter 0.03
instance arrayComponentEndCap :: ArrayComponent EndCap where
    arrayNumber = view _arrayNumber
instance protoDecodableEndCap :: ProtoDecodable EndCap EndCapPB where
    fromProto e = EndCap {
        id          : fromProto $ getUUID e,
        x           : meter $ getX e,
        y           : meter $ getY e,
        z           : meter $ getZ e,
        arrayNumber : getArrayNumber e,
        skirtId     : fromProto $ getSkirt e,
        position    : fromProto $ getPos e
    }