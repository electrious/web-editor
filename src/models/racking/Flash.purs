module Model.Racking.Flash where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Lens (view, (.~))
import Data.Meter (Meter, inch)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)


newtype Flash = Flash {
    id          :: UUID,
    rafterId    :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int
}

derive instance Newtype Flash _
derive instance Generic Flash _
instance showFlash :: Show Flash where
    show = genericShow
instance RoofComponent Flash where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 1.0
                 # _height .~ inch 1.0
instance ArrayComponent Flash where
    arrayNumber = view _arrayNumber
instance EncodeJson Flash where
    encodeJson (Flash f) = "id"  := f.id
                        ~> "rid" := f.rafterId
                        ~> "x"   := f.x
                        ~> "y"   := f.y
                        ~> "z"   := f.z
                        ~> "an"  := f.arrayNumber
                        ~> jsonEmptyObject
instance DecodeJson Flash where
    decodeJson = decodeJson >=> f
        where f o = mkFlash <$> o .: "id"
                            <*> o .: "an"
                            <*> o .: "rid"
                            <*> o .: "x"
                            <*> o .: "y"
                            <*> o .: "z"

mkFlash :: UUID -> Int -> UUID -> Meter -> Meter -> Meter -> Flash
mkFlash id arrayNumber rafterId x y z = Flash { id : id, arrayNumber : arrayNumber, rafterId : rafterId, x : x, y : y, z : z }