module Model.Roof.ArrayConfig where

import Prelude hiding (degree)

import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Meter (Meter, inch, meter)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Math.Angle (Angle, degree)
import Model.Racking.RackingType (RackingType(..))

newtype ArrayConfig = ArrayConfig {
    rackingType  :: RackingType,
    panelSlope   :: Angle,      -- panel slope on a flat roof
    panelLowestZ :: Meter,      -- z of the lowest point of a panel
    gapX         :: Meter,
    gapY         :: Meter
}

derive instance newtypeArrayConfig :: Newtype ArrayConfig _
derive instance genericArrayConfig :: Generic ArrayConfig _
instance showArrayConfig :: Show ArrayConfig where
    show = genericShow
instance defaultArrayConfig :: Default ArrayConfig where
    def = ArrayConfig {
                rackingType  : XR10,
                panelSlope   : degree 0.0,
                panelLowestZ : meter 0.12,
                gapX         : inch 0.5,
                gapY         : inch 0.75
            }

_rackingType :: Lens' ArrayConfig RackingType
_rackingType = _Newtype <<< prop (SProxy :: SProxy "rackingType")

_panelSlope :: Lens' ArrayConfig Angle
_panelSlope = _Newtype <<< prop (SProxy :: SProxy "panelSlope")

_panelLowestZ :: Lens' ArrayConfig Meter
_panelLowestZ = _Newtype <<< prop (SProxy :: SProxy "panelLowestZ")

_gapX :: Lens' ArrayConfig Meter
_gapX = _Newtype <<< prop (SProxy :: SProxy "gapX")

_gapY :: Lens' ArrayConfig Meter
_gapY = _Newtype <<< prop (SProxy :: SProxy "gapY")

fxArrayConfig :: ArrayConfig
fxArrayConfig = def # _rackingType .~ FX

xr10ArrayConfig :: ArrayConfig
xr10ArrayConfig = def

-- Global constant for row gap value in XR Flat system
xrFlatRowGap :: Meter
xrFlatRowGap = inch 30.0

xrFlatArrayConfig :: ArrayConfig
xrFlatArrayConfig = def # _rackingType  .~ XRFlat
                        # _panelSlope   .~ degree 18.0
                        # _panelLowestZ .~ meter 0.2
                        # _gapX         .~ inch 0.5
                        # _gapY         .~ xrFlatRowGap

bxArrayConfig :: ArrayConfig
bxArrayConfig = def # _rackingType  .~ BX
                    # _panelSlope   .~ degree 10.0
                    # _panelLowestZ .~ meter 0.1
                    # _gapX         .~ inch 0.5
                    # _gapY         .~ inch 13.0 -- 10 inch for 5 degrees

gafArrayConfig :: ArrayConfig
gafArrayConfig = def # _rackingType  .~ GAF
                     # _panelSlope   .~ degree 3.0
                     # _panelLowestZ .~ meter 0.01
                     # _gapX         .~ meter 0.05
                     # _gapY         .~ meter 0.05


arrayConfigForRack :: RackingType -> ArrayConfig
arrayConfigForRack FX     = fxArrayConfig
arrayConfigForRack XR10   = xr10ArrayConfig
arrayConfigForRack XRFlat = xrFlatArrayConfig
arrayConfigForRack BX     = bxArrayConfig
arrayConfigForRack GAF    = gafArrayConfig
