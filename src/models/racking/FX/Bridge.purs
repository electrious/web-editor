module Model.Racking.FX.Bridge where

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

bridgeWidth :: Meter
bridgeWidth = inch 8.0

newtype Bridge = Bridge {
    id :: UUID,
    x  :: Meter,
    y  :: Meter,
    z  :: Meter,
    arrayNumber :: Int
}

derive instance Newtype Bridge _
derive instance Generic Bridge _
instance showBridge :: Show Bridge where
    show = genericShow
instance RoofComponent Bridge where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 1.0
                 # _height .~ inch 1.0
instance ArrayComponent Bridge where
    arrayNumber = view _arrayNumber
instance EncodeJson Bridge where
    encodeJson (Bridge b) = "id" := b.id
                         ~> "x"  := b.x
                         ~> "y"  := b.y
                         ~> "z"  := b.z
                         ~> "an" := b.arrayNumber
instance DecodeJson Bridge where
    decodeJson = decodeJson >=> f
        where f o = mkBridge <$> o .: "id"
                             <*> o .: "an"
                             <*> o .: "x"
                             <*> o .: "y"
                             <*> o .: "z"

mkBridge :: UUID -> Int -> Meter -> Meter -> Meter -> Bridge
mkBridge id arrayNumber x y z = Bridge {id : id, arrayNumber: arrayNumber, x: x, y: y, z: z }