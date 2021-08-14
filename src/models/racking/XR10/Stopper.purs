module Model.Racking.XR10.Stopper where

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

newtype Stopper = Stopper {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    type        :: RackPos
}

derive instance Newtype Stopper _
derive instance Generic Stopper _
instance Show Stopper where
    show = genericShow
instance RoofComponent Stopper where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ meter 0.04
                 # _height .~ meter 0.04
instance ArrayComponent Stopper where
    arrayNumber = view _arrayNumber
instance EncodeJson Stopper where
    encodeJson (Stopper s) = "id" := s.id
                          ~> "an" := s.arrayNumber
                          ~> "x"  := s.x
                          ~> "y"  := s.y
                          ~> "z"  := s.z
                          ~> "t"  := s.type
                          ~> jsonEmptyObject
instance DecodeJson Stopper where
    decodeJson = decodeJson >=> f
        where f o = mkStopper <$> o .: "id"
                              <*> o .: "an"
                              <*> o .: "x"
                              <*> o .: "y"
                              <*> o .: "z"
                              <*> o .: "t"

mkStopper :: UUID -> Int -> Meter -> Meter -> Meter -> RackPos -> Stopper
mkStopper id arrayNumber x y z t = Stopper { id: id, arrayNumber: arrayNumber, x: x, y: y, z: z, type: t }