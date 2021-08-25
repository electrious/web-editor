module Model.Racking.XRFlat.QBaseMount where

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

newtype QBaseMount = QBaseMount {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    height      :: Meter
}

derive instance Newtype QBaseMount _
derive instance Generic QBaseMount _
instance Show QBaseMount where
    show = genericShow
instance RoofComponent QBaseMount where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 1.0
                 # _height .~ inch 1.0
instance ArrayComponent QBaseMount where
    arrayNumber = view _arrayNumber
instance EncodeJson QBaseMount where
    encodeJson (QBaseMount m) = "id" := m.id
                             ~> "an" := m.arrayNumber
                             ~> "x"  := m.x
                             ~> "y"  := m.y
                             ~> "z"  := m.z
                             ~> "h"  := m.height
                             ~> jsonEmptyObject
instance DecodeJson QBaseMount where
    decodeJson = decodeJson >=> f
        where f o = mkQBaseMount <$> o .: "id"
                                 <*> o .: "an"
                                 <*> o .: "x"
                                 <*> o .: "y"
                                 <*> o .: "z"
                                 <*> o .: "h"

mkQBaseMount :: UUID -> Int -> Meter -> Meter -> Meter -> Meter -> QBaseMount
mkQBaseMount id arrayNumber x y z height = QBaseMount { id: id, x: x, y: y, z: z, arrayNumber: arrayNumber, height: height }