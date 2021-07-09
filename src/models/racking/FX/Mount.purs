module Model.Racking.FX.Mount where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (Lens', view, (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Meter (Meter, inch, meter)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.ArrayComponent (class ArrayComponent)
import Model.Class (class HasPBUUID, class IsPBArrayComp, getArrayNumber, getUUID, getX, getY, getZ)
import Model.RoofComponent (class RoofComponent)
import Model.UUID (PBUUID)

foreign import data MountPB :: Type
foreign import mkMountPB :: Effect MountPB

instance hasPBUUIdMountPB :: HasPBUUID MountPB
instance isPBArrayCompMountPB :: IsPBArrayComp MountPB

foreign import getFlash :: MountPB -> PBUUID
foreign import setFlash :: PBUUID -> MountPB -> Effect Unit
foreign import getClampX :: MountPB -> Number
foreign import setClampX :: Number -> MountPB -> Effect Unit

mountRadius :: Meter
mountRadius = inch 5.0

mountLength :: Meter
mountLength = meter 0.11

mountWidth :: Meter
mountWidth = meter 0.0508

newtype Mount = Mount {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    flashId     :: UUID,
    clampX      :: Meter
}

derive instance newtypeMount :: Newtype Mount _
derive instance genericMount :: Generic Mount _
instance showMount :: Show Mount where
    show = genericShow
instance roofComponentMount :: RoofComponent Mount where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ mountWidth
                 # _height .~ meter 0.1
instance arrayComponentMount :: ArrayComponent Mount where
    arrayNumber = view _arrayNumber
instance protoDecodableMount :: ProtoDecodable Mount MountPB where
    fromProto m = Mount {
        id          : fromProto $ getUUID m,
        x           : meter $ getX m,
        y           : meter $ getY m,
        z           : meter $ getZ m,
        arrayNumber : getArrayNumber m,
        flashId     : fromProto $ getFlash m,
        clampX      : meter $ getClampX m
    }

_clampX :: Lens' Mount Meter
_clampX = _Newtype <<< prop (SProxy :: SProxy "clampX")
