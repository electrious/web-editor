module Model.Racking.XR10.Rail where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Lens (view, (^.), (.~))
import Data.Meter (Meter, feetInch, inch)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _length, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)


railLength :: Meter
railLength = feetInch 14.0 0.0

newtype Rail = Rail {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter,
    panelIds    :: Array UUID
}

derive instance Newtype Rail _
derive instance Generic Rail _
instance Show Rail where
    show = genericShow
instance RoofComponent Rail where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size r = def # _width  .~ r ^. _length
                 # _height .~ inch 1.0
instance ArrayComponent Rail where
    arrayNumber = view _arrayNumber
instance EncodeJson Rail where
    encodeJson (Rail r) = "id"  := r.id
                       ~> "an"  := r.arrayNumber
                       ~> "x"   := r.x
                       ~> "y"   := r.y
                       ~> "z"   := r.z
                       ~> "l"   := r.length
                       ~> "pid" := r.panelIds
                       ~> jsonEmptyObject
instance DecodeJson Rail where
    decodeJson = decodeJson >=> f
        where f o = mkRail <$> o .: "id"
                           <*> o .: "an"
                           <*> o .: "x"
                           <*> o .: "y"
                           <*> o .: "z"
                           <*> o .: "l"
                           <*> o .: "pid"

mkRail :: UUID -> Int -> Meter -> Meter -> Meter -> Meter -> Array UUID -> Rail
mkRail id arrayNumber x y z length panelIds = Rail { id: id, x: x, y: y, z: z, arrayNumber: arrayNumber, length: length, panelIds: panelIds }