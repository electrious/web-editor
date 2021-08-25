module Model.Racking.BX.BXRoofParameter where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Model.Racking.BX.Chassis (ChassisType)
import Type.Proxy (Proxy(..))


newtype BXRoofParameter = BXRoofParameter {
    chassisType :: ChassisType
}

derive instance Newtype BXRoofParameter _
derive instance Generic BXRoofParameter _
instance Show BXRoofParameter where
    show = genericShow
instance EncodeJson BXRoofParameter where
    encodeJson p = "ct" := p ^. _chassisType
                ~> jsonEmptyObject
instance DecodeJson BXRoofParameter where
    decodeJson = decodeJson >=> f
        where f o = mkBXRoofParameter <$> o .: "ct"

_chassisType :: forall t a r. Newtype t { chassisType :: a | r } => Lens' t a
_chassisType = _Newtype <<< prop (Proxy :: Proxy "chassisType")

mkBXRoofParameter :: ChassisType -> BXRoofParameter
mkBXRoofParameter t = BXRoofParameter { chassisType : t }