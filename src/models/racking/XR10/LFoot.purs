module Model.Racking.XR10.LFoot where

import Prelude

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

newtype LFoot = LFoot {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    flashId     :: UUID
}

derive instance Newtype LFoot _
derive instance Generic LFoot _
instance Show LFoot where
    show = genericShow
instance RoofComponent LFoot where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 1.0
                 # _height .~ inch 1.0
instance ArrayComponent LFoot where
    arrayNumber = view _arrayNumber
instance EncodeJson LFoot where
    encodeJson (LFoot l) = "id"  := l.id
                        ~> "x"   := l.x
                        ~> "y"   := l.y
                        ~> "z"   := l.z
                        ~> "an"  := l.arrayNumber
                        ~> "fid" := l.flashId
instance DecodeJson LFoot where
    decodeJson = decodeJson >=> f
        where f o = mkLFoot <$> o .: "id"
                            <*> o .: "an"
                            <*> o .: "fid"
                            <*> o .: "x"
                            <*> o .: "y"
                            <*> o .: "z"

mkLFoot :: UUID -> Int -> UUID -> Meter -> Meter -> Meter -> LFoot
mkLFoot id arrayNumber flashId x y z = LFoot { id : id, arrayNumber : arrayNumber, flashId : flashId, x : x, y : y, z : z }