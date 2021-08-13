module Model.Racking.FX.EndCap where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Lens (view, (.~))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.Racking.Common (RackPos)
import Model.RoofComponent (class RoofComponent)

newtype EndCap = EndCap {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    skirtId     :: UUID,
    position    :: RackPos
}

derive instance newtypeEndCap :: Newtype EndCap _
derive instance genericEndCap :: Generic EndCap _
instance showEndCap :: Show EndCap where
    show = genericShow
instance roofComponentEndCap :: RoofComponent EndCap where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ meter 0.001
                 # _height .~ meter 0.03
instance arrayComponentEndCap :: ArrayComponent EndCap where
    arrayNumber = view _arrayNumber
instance EncodeJson EndCap where
    encodeJson (EndCap c) = "id"  := c.id
                         ~> "x"   := c.x
                         ~> "y"   := c.y
                         ~> "z"   := c.z
                         ~> "an"  := c.arrayNumber
                         ~> "sid" := c.skirtId
                         ~> "pos" := c.position
                         ~> jsonEmptyObject
instance DecodeJson EndCap where
    decodeJson = decodeJson >=> f
        where f o = mkEndCap <$> o .: "id"
                             <*> o .: "an"
                             <*> o .: "sid"
                             <*> o .: "x"
                             <*> o .: "y"
                             <*> o .: "z"
                             <*> o .: "pos"

mkEndCap :: UUID -> Int -> UUID -> Meter -> Meter -> Meter -> RackPos -> EndCap
mkEndCap id an sid x y z pos = EndCap { id: id, arrayNumber: an, skirtId: sid, x: x, y: y, z: z, position: pos }