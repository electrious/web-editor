module Model.RoofSpecific where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Data.UUIDWrapper (UUID)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

newtype RoofSpecific a = RoofSpecific {
    roofId :: UUID,
    value  :: a
}

derive instance newtypeRoofSpecific :: Newtype (RoofSpecific a) _
derive instance genericRoofSpecific :: Generic (RoofSpecific a) _

instance showRoofSpecific :: Show a => Show (RoofSpecific a) where
    show = genericShow
instance eqRoofSpecific :: Eq a => Eq (RoofSpecific a) where
    eq = genericEq
instance encodeRoofSpecific :: Encode a => Encode (RoofSpecific a) where
    encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
instance decodeRoofSpecific :: Decode a => Decode (RoofSpecific a) where
    decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance functorRoofSpecific :: Functor RoofSpecific where
    map f (RoofSpecific r) = RoofSpecific $ r { value = f r.value }

_value :: forall t a r. Newtype t { value :: a | r } => Lens' t a
_value = _Newtype <<< prop (Proxy :: Proxy "value")

mkRoofSpecific :: forall a. UUID -> a -> RoofSpecific a
mkRoofSpecific i v = RoofSpecific {
    roofId : i,
    value  : v
}
