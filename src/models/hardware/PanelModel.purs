module Model.Hardware.PanelModel where

import Prelude

import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Number.Format (fixed, toStringWith)
import Data.String (drop)
import Type.Proxy (Proxy(..))
import Editor.Common.Lenses (_name)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

newtype PanelModel = PanelModel {
    id           :: Int,
    name         :: String,
    power        :: Int,
    isDefault    :: Boolean,
    isActive     :: Boolean,
    pricePerWatt :: Number,
    sizeShort    :: Number,
    sizeLong     :: Number
}

derive instance newtypePanelModel :: Newtype PanelModel _
derive instance genericPanelModel :: Generic PanelModel _
instance showPanelModel :: Show PanelModel where
    show = genericShow
instance defaultPanelModel :: Default PanelModel where
    def = PanelModel {
        id           : 1,
        name         : "Premium Panel - Yingli 305W",
        power        : 305,
        isDefault    : true,
        isActive     : true,
        pricePerWatt : 0.0,
        sizeShort    : 1.0,
        sizeLong     : 1.6
    }
derive instance eqPanelModel :: Eq PanelModel
instance ordPanelModel :: Ord PanelModel where
    compare = genericCompare
instance encodePanelModel :: Encode PanelModel where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true, fieldTransform = fieldTrans })
instance decodePanelModel :: Decode PanelModel where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true, fieldTransform = fieldTrans })

fieldTrans :: String -> String
fieldTrans "id"           = "id"
fieldTrans "name"         = "name"
fieldTrans "power"        = "power"
fieldTrans "isDefault"    = "default"
fieldTrans "isActive"     = "active"
fieldTrans "pricePerWatt" = "price_per_watt"
fieldTrans "sizeShort"    = "size_short"
fieldTrans "sizeLong"     = "size_long"
fieldTrans _              = ""

_power :: forall t a r. Newtype t { power :: a | r } => Lens' t a
_power = _Newtype <<< prop (Proxy :: Proxy "power")

_isDefault :: forall t a r. Newtype t { isDefault :: a | r } => Lens' t a
_isDefault = _Newtype <<< prop (Proxy :: Proxy "isDefault")

_pricePerWatt :: forall t a r. Newtype t { pricePerWatt :: a | r } => Lens' t a
_pricePerWatt = _Newtype <<< prop (Proxy :: Proxy "pricePerWatt")

_sizeShort :: forall t a r. Newtype t { sizeShort :: a | r } => Lens' t a
_sizeShort = _Newtype <<< prop (Proxy :: Proxy "sizeShort")

_sizeLong :: forall t a r. Newtype t { sizeLong :: a | r } => Lens' t a
_sizeLong = _Newtype <<< prop (Proxy :: Proxy "sizeLong")

priceString :: PanelModel -> String
priceString pm = if p > 0.0 && p < 1.0
                 then drop 1 s
                 else s
    where p = pm ^. _pricePerWatt
          s = toStringWith (fixed 2) p

panelModelLabel :: PanelModel -> String
panelModelLabel pm = if pm ^. _pricePerWatt > 0.0
                     then pm ^. _name <> "   +$" <> priceString pm <> "/W"
                     else pm ^. _name
