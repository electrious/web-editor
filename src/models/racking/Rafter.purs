module Model.Racking.Rafter where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (view, (.~), (^.))
import Data.Meter (Meter, inch, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_height, _id, _length, _width, _x, _y, _z)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Class (class HasLength, class HasPBUUID, class IsPBArrayComp, getLength, getUUID, getX, getY, getZ)
import Model.RoofComponent (class RoofComponent)

foreign import data RafterPB :: Type

foreign import mkRafterPB :: Effect RafterPB

instance hasPBUUIDRafterPB :: HasPBUUID RafterPB
instance isPBArrayCompRafterPB :: IsPBArrayComp RafterPB
instance hasLengthRafterPB :: HasLength RafterPB

newtype Rafter = Rafter {
    id     :: UUID,
    x      :: Meter,
    y      :: Meter,
    z      :: Meter,
    length :: Meter
}

derive instance newtypeRafter :: Newtype Rafter _
derive instance genericRafter :: Generic Rafter _
instance showRafter :: Show Rafter where
    show = genericShow
instance roofComponentRafter :: RoofComponent Rafter where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size r = def # _width  .~ inch 1.0
                 # _height .~ r ^. _length
instance protoDecodableRafter :: ProtoDecodable Rafter RafterPB where
    fromProto r = Rafter {
        id     : fromProto $ getUUID r,
        x      : meter $ getX r,
        y      : meter $ getY r,
        z      : meter $ getZ r,
        length : meter $ getLength r
    }
