module Model.RoofSpecific where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import Type.Proxy (Proxy(..))

newtype RoofSpecific a = RoofSpecific {
    roofId :: UUID,
    value  :: a
}

derive instance Newtype (RoofSpecific a) _
derive instance Generic (RoofSpecific a) _

instance Show a => Show (RoofSpecific a) where
    show = genericShow
instance Eq a => Eq (RoofSpecific a) where
    eq = genericEq
instance EncodeJson a => EncodeJson (RoofSpecific a) where
    encodeJson (RoofSpecific r) = "roofId" := r.roofId
                               ~> "value" := r.value
                               ~> jsonEmptyObject
instance DecodeJson a => DecodeJson (RoofSpecific a) where
    decodeJson = decodeJson >=> f
        where f o = mkRoofSpecific <$> o .: "roofId"
                                   <*> o .: "value"

instance Functor RoofSpecific where
    map f (RoofSpecific r) = RoofSpecific $ r { value = f r.value }

_value :: forall t a r. Newtype t { value :: a | r } => Lens' t a
_value = _Newtype <<< prop (Proxy :: Proxy "value")

mkRoofSpecific :: forall a. UUID -> a -> RoofSpecific a
mkRoofSpecific i v = RoofSpecific {
    roofId : i,
    value  : v
}
