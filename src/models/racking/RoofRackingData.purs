module Model.Racking.RoofRackingData where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDMap (fromObject, toObject)
import Model.Racking.BX.BXRackingComponent (BXRackingComponent, BXRackingNumbers)
import Model.Racking.FX.FXRackingComponent (FXRackingComponent, FXRackingNumbers)
import Model.Racking.GAF.GAFRackingComponent (GAFRackingComponent, GAFRackingNumbers)
import Model.Racking.RackingType (RackingType)
import Model.Racking.Rafter (Rafter)
import Model.Racking.RoofParameter (RoofParameter, candidate)
import Model.Racking.XR10.XRRackingComponent (XRRackingComponent, XRRackingNumbers)
import Model.Racking.XRFlat.XRFlatRackingComponent (XRFlatRackingComponent, XRFlatRackingNumbers)
import Type.Proxy (Proxy(..))

data RackingComp = FX FXRackingComponent
                 | XR XRRackingComponent
                 | XRFlat XRFlatRackingComponent
                 | BX BXRackingComponent
                 | GAF GAFRackingComponent

derive instance Generic RackingComp _
instance Show RackingComp where
    show = genericShow
instance Default RackingComp where
    def = XR def
instance EncodeJson RackingComp where
    encodeJson (FX c)     = "fx"  := c ~> jsonEmptyObject
    encodeJson (XR c)     = "xr"  := c ~> jsonEmptyObject
    encodeJson (XRFlat c) = "fl"  := c ~> jsonEmptyObject
    encodeJson (BX c)     = "bx"  := c ~> jsonEmptyObject
    encodeJson (GAF c)    = "gaf" := c ~> jsonEmptyObject
instance DecodeJson RackingComp where
    decodeJson = decodeJson >=> f
        where f o = do
                fx  <- map FX     <$> o .:? "fx"
                xr  <- map XR     <$> o .:? "xr"
                fl  <- map XRFlat <$> o .:? "fl"
                bx  <- map BX     <$> o .:? "bx"
                gaf <- map GAF    <$> o .:? "gaf"
                pure $ candidate [fx, xr, fl, bx, gaf]

data RackingCompNumbers = FXNum FXRackingNumbers
                        | XRNum XRRackingNumbers
                        | XRFlatNum XRFlatRackingNumbers
                        | BXNum BXRackingNumbers
                        | GAFNum GAFRackingNumbers

derive instance Generic RackingCompNumbers _
instance Show RackingCompNumbers where
    show = genericShow

newtype RoofRackingData = RoofRackingData {
    rackingType :: RackingType,
    rafters     :: Array Rafter,
    parameters  :: RoofParameter,
    arrayComps  :: Map Int RackingComp
}

derive instance Newtype RoofRackingData _
derive instance Generic RoofRackingData _
instance Show RoofRackingData where
    show = genericShow
instance EncodeJson RoofRackingData where
    encodeJson (RoofRackingData d) = "t"   := d.rackingType
                                  ~> "rs"  := d.rafters
                                  ~> "prm" := d.parameters
                                  ~> "dat" := toObject d.arrayComps
instance DecodeJson RoofRackingData where
    decodeJson = decodeJson >=> f
        where f o = mkRoofRackingData <$> o .: "t"
                                      <*> o .: "rs"
                                      <*> o .: "prm"
                                      <*> (fromObject <$> o .: "dat")

mkRoofRackingData :: RackingType -> Array Rafter -> RoofParameter -> Map Int RackingComp -> RoofRackingData
mkRoofRackingData t rs prm dat = RoofRackingData { rackingType : t, rafters: rs, parameters: prm, arrayComps: dat }

_rafters :: forall t a r. Newtype t { rafters :: a | r } => Lens' t a
_rafters = _Newtype <<< prop (Proxy :: Proxy "rafters")

_arrayComps :: forall t a r. Newtype t { arrayComps :: a | r } => Lens' t a
_arrayComps = _Newtype <<< prop (Proxy :: Proxy "arrayComps")