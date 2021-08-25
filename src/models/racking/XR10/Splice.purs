module Model.Racking.XR10.Splice where

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

newtype Splice = Splice {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    railIds     :: Array UUID
}

derive instance Newtype Splice _
derive instance Generic Splice _
instance Show Splice where
    show = genericShow
instance RoofComponent Splice where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 7.0
                 # _height .~ inch 1.0
instance ArrayComponent Splice where
    arrayNumber = view _arrayNumber
instance EncodeJson Splice where
    encodeJson (Splice s) = "id"  := s.id
                         ~> "an"  := s.arrayNumber
                         ~> "x"   := s.x
                         ~> "y"   := s.y
                         ~> "z"   := s.z
                         ~> "rid" := s.railIds
                         ~> jsonEmptyObject
instance DecodeJson Splice where
    decodeJson = decodeJson >=> f
        where f o = mkSplice <$> o .: "id"
                             <*> o .: "an"
                             <*> o .: "x"
                             <*> o .: "y"
                             <*> o .: "z"
                             <*> o .: "rid"

mkSplice :: UUID -> Int -> Meter -> Meter -> Meter -> Array UUID -> Splice
mkSplice id arrayNumber x y z railIds = Splice { id: id, arrayNumber: arrayNumber, x: x, y: y, z: z, railIds: railIds }