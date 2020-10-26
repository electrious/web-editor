module Model.Racking.Flash where

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

foreign import data FlashPB :: Type
foreign import mkFlashPB :: Effect FlashPB

instance hasPBUUIDFlashPB :: HasPBUUID FlashPB
instance isPBArrayCompFlashPB :: IsPBArrayComp FlashPB

foreign import getRafterId :: FlashPB -> PBUUID
foreign import setRafterId :: PBUUID -> FlashPB -> Effect Unit
foreign import getClampTarget :: FlashPB -> Number
foreign import setClampTarget :: Number -> FlashPB -> Effect Unit

newtype Flash = Flash {
    id          :: UUID,
    rafterId    :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int
}

derive instance newtypeFlash :: Newtype Flash _
derive instance genericFlash :: Generic Flash _
instance showFlash :: Show Flash where
    show = genericShow
instance roofComponentFlash :: RoofComponent Flash where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 1.0
                 # _height .~ inch 1.0
instance arrayComponentFlash :: ArrayComponent Flash where
    arrayNumber = view _arrayNumber
instance protoDecodableFlash :: ProtoDecodable Flash FlashPB where
    fromProto f = Flash {
        id          : fromProto $ getUUID f,
        rafterId    : fromProto $ getRafterId f,
        x           : meter $ getX f,
        y           : meter $ getY f,
        z           : meter $ getZ f,
        arrayNumber : getArrayNumber f
    }