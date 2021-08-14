module Model.Racking.FX.FXRoofParameter where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Model.Racking.MountSpacing (MountSpacing)
import Model.Racking.RafterSpacing (RafterSpacing)

newtype FXRoofParameter = FXRoofParameter {
    mountSpacing  :: MountSpacing,
    rafterSpacing :: RafterSpacing
}

derive instance newtypeFXRoofParameter :: Newtype FXRoofParameter _
derive instance genericFXRoofParameter :: Generic FXRoofParameter _
instance showFXRoofParameter :: Show FXRoofParameter where
    show = genericShow
instance EncodeJson FXRoofParameter where
    encodeJson (FXRoofParameter p) = "ms" := p.mountSpacing
                                  ~> "rs" := p.rafterSpacing
                                  ~> jsonEmptyObject
instance DecodeJson FXRoofParameter where
    decodeJson = decodeJson >=> f
        where f o = mkFXRoofParameter <$> o .: "ms"
                                      <*> o .: "rs"


mkFXRoofParameter :: MountSpacing -> RafterSpacing -> FXRoofParameter
mkFXRoofParameter ms rs = FXRoofParameter { mountSpacing : ms, rafterSpacing : rs }