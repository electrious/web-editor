module Model.Racking.XRFlat.QBaseMount where

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

foreign import data QBaseMountPB :: Type
foreign import mkQBaseMountPB :: Effect QBaseMountPB

instance hasPBUUIDQBaseMountPB :: HasPBUUID QBaseMountPB
instance isPBArrayCompQBaseMountPB :: IsPBArrayComp QBaseMountPB

foreign import getHeight :: QBaseMountPB -> Number
foreign import setHeight :: Number -> QBaseMountPB -> Effect Unit

newtype QBaseMount = QBaseMount {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    height      :: Meter
}

derive instance newtypeQBaseMount :: Newtype QBaseMount _
derive instance genericQBaseMount :: Generic QBaseMount _
instance showQBaseMount :: Show QBaseMount where
    show = genericShow
instance roofComponentQBaseMount :: RoofComponent QBaseMount where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 1.0
                 # _height .~ inch 1.0
instance arrayComponentQBaseMount :: ArrayComponent QBaseMount where
    arrayNumber = view _arrayNumber
instance protoDecodableQBaseMount :: ProtoDecodable QBaseMount QBaseMountPB where
    fromProto q = QBaseMount {
        id          : fromProto $ getUUID q,
        x           : meter $ getX q,
        y           : meter $ getY q,
        z           : meter $ getZ q,
        arrayNumber : getArrayNumber q,
        height      : meter $ getHeight q
    }