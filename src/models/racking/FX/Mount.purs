module Model.Racking.FX.Mount where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', view, (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Meter (Meter, inch, meter)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)
import Type.Proxy (Proxy(..))

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
instance EncodeJson Mount where
    encodeJson (Mount m) = "id"  := m.id
                        ~> "x"   := m.x
                        ~> "y"   := m.y
                        ~> "z"   := m.z
                        ~> "an"  := m.arrayNumber
                        ~> "fid" := m.flashId
                        ~> "cx"  := m.clampX
                        ~> jsonEmptyObject
instance DecodeJson Mount where
    decodeJson = decodeJson >=> f
        where f o = mkMount <$> o .: "id"
                            <*> o .: "an"
                            <*> o .: "fid"
                            <*> o .: "x"
                            <*> o .: "y"
                            <*> o .: "z"
                            <*> o .: "cx"

mkMount :: UUID -> Int -> UUID -> Meter -> Meter -> Meter -> Meter -> Mount
mkMount id arrayNumber flashId x y z clampX = Mount { id: id, arrayNumber: arrayNumber, flashId: flashId, x: x, y: y, z: z, clampX: clampX  }

_clampX :: Lens' Mount Meter
_clampX = _Newtype <<< prop (Proxy :: Proxy "clampX")
