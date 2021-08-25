module Model.Racking.XRFlat.SupportRail where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Lens (view, (^.), (.~))
import Data.Meter (Meter, inch)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _length, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)

newtype SupportRail = SupportRail {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter
}

derive instance Newtype SupportRail _
derive instance Generic SupportRail _
instance Show SupportRail where
    show = genericShow
instance RoofComponent SupportRail where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size s = def # _width  .~ inch 1.0
                 # _height .~ s ^. _length
instance ArrayComponent SupportRail where
    arrayNumber = view _arrayNumber
instance EncodeJson SupportRail where
    encodeJson (SupportRail r) = "id" := r.id
                              ~> "an" := r.arrayNumber
                              ~> "x"  := r.x
                              ~> "y"  := r.y
                              ~> "z"  := r.z
                              ~> "l"  := r.length
                              ~> jsonEmptyObject
instance DecodeJson SupportRail where
    decodeJson = decodeJson >=> f 
        where f o = mkSupportRail <$> o .: "id"
                                  <*> o .: "an"
                                  <*> o .: "x"
                                  <*> o .: "y"
                                  <*> o .: "z"
                                  <*> o .: "l"

mkSupportRail :: UUID -> Int -> Meter -> Meter -> Meter -> Meter -> SupportRail
mkSupportRail id arrayNumber x y z length = SupportRail { id: id, arrayNumber: arrayNumber, x: x, y: y, z: z, length: length }