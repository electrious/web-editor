module Model.Racking.BX.Block where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Lens (view, (.~), (^.))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)

newtype Block = Block {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int
}

derive instance Newtype Block _
derive instance Generic Block _
instance showBlock :: Show Block where
    show = genericShow
instance RoofComponent Block where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ meter 0.2
                 # _height .~ meter 0.2
instance ArrayComponent Block where
    arrayNumber = view _arrayNumber
instance EncodeJson Block where
    encodeJson b = "id" := b ^. _id
                 ~> "x" := b ^. _x
                 ~> "y" := b ^. _y
                 ~> "z" := b ^. _z
                 ~> "an" := b ^. _arrayNumber
                 ~> jsonEmptyObject
instance DecodeJson Block where
    decodeJson json = do
        obj <- decodeJson json
        mkBlock <$> obj .: "id"
                <*> obj .: "an"
                <*> obj .: "x"
                <*> obj .: "y"
                <*> obj .: "z"

mkBlock :: UUID -> Int -> Meter -> Meter -> Meter -> Block
mkBlock i an x y z = Block { id: i, arrayNumber: an, x: x, y: y, z: z }
