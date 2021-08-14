module Model.Racking.XR.XRRoofParameter where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Model.Racking.MountSpacing (MountSpacing(..))
import Model.Racking.RafterSpacing (RafterSpacing(..))


newtype XRRoofParameter = XRRoofParameter {
    mountSpacing  :: MountSpacing,
    rafterSpacing :: RafterSpacing
}

derive instance Newtype XRRoofParameter _
derive instance Generic XRRoofParameter _
instance Show XRRoofParameter where
    show = genericShow
instance Default XRRoofParameter where
    def = mkXRRoofParameter MountSpacing64 RafterSpacing24
instance EncodeJson XRRoofParameter where
    encodeJson (XRRoofParameter p) = "ms" := p.mountSpacing
                                  ~> "rs" := p.rafterSpacing
                                  ~> jsonEmptyObject
instance DecodeJson XRRoofParameter where
    decodeJson = decodeJson >=> f
        where f o = mkXRRoofParameter <$> o .: "ms"
                                      <*> o .: "rs"

mkXRRoofParameter :: MountSpacing -> RafterSpacing -> XRRoofParameter
mkXRRoofParameter ms rs = XRRoofParameter { mountSpacing : ms, rafterSpacing : rs }