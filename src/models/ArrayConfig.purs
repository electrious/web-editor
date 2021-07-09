module Model.Roof.ArrayConfig where

import Prelude hiding (degree)

import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Meter (Meter, inch, meter, meterVal)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Editor.Common.Lenses (_height, _rackingType, _width)
import Math.Angle (Angle, cos, degree)
import Model.Racking.RackingType (RackingType(..))
import Model.Roof.Panel (Panel)
import Model.RoofComponent (size)

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

_panelSlope :: Lens' ArrayConfig Angle
_panelSlope = _Newtype <<< prop (Proxy :: Proxy "panelSlope")

_panelLowestZ :: Lens' ArrayConfig Meter
_panelLowestZ = _Newtype <<< prop (Proxy :: Proxy "panelLowestZ")

_gapX :: forall t a r. Newtype t { gapX :: a | r } => Lens' t a
_gapX = _Newtype <<< prop (Proxy :: Proxy "gapX")

_gapY :: forall t a r. Newtype t { gapY :: a | r } => Lens' t a
_gapY = _Newtype <<< prop (Proxy :: Proxy "gapY")

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


-- distance between center of two nearby rows of panels
rowDistance :: ArrayConfig -> Panel -> Meter
rowDistance cfg p = meter $ h * cos s + gapY
    where s    = cfg ^. _panelSlope
          gapY = meterVal $ cfg ^. _gapY
          h    = meterVal $ (size p) ^. _height

colDistance :: ArrayConfig -> Panel -> Meter
colDistance cfg p = meter $ w + gapX
    where w    = meterVal $ (size p) ^. _width
          gapX = meterVal $ cfg ^. _gapX
