module Model.Racking.Rafter where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Lens (view, (.~), (^.))
import Data.Meter (Meter, inch)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_height, _id, _length, _width, _x, _y, _z)
import Model.RoofComponent (class RoofComponent)


newtype Rafter = Rafter {
    id     :: UUID,
    x      :: Meter,
    y      :: Meter,
    z      :: Meter,
    length :: Meter
}

derive instance Newtype Rafter _
derive instance Generic Rafter _
instance Show Rafter where
    show = genericShow
instance RoofComponent Rafter where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size r = def # _width  .~ inch 1.0
                 # _height .~ r ^. _length
instance EncodeJson Rafter where
    encodeJson (Rafter r) = "id" := r.id
                         ~> "x"  := r.x
                         ~> "y"  := r.y
                         ~> "z"  := r.z
                         ~> "l"  := r.length
                         ~> jsonEmptyObject
instance DecodeJson Rafter where
    decodeJson = decodeJson >=> f
        where f o = mkRafter <$> o .: "id"
                             <*> o .: "x"
                             <*> o .: "y"
                             <*> o .: "z"
                             <*> o .: "l"

mkRafter :: UUID -> Meter -> Meter -> Meter -> Meter -> Rafter
mkRafter id x y z l = Rafter { id : id, x : x, y : y, z : z, length : l }