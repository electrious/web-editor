module Data.Hardware.Size where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))

-- | Size type to represent size of real world object in meters
newtype Size = Size {
    width  :: Meter,
    height :: Meter
}

derive instance newtypeSize :: Newtype Size _

_width :: Lens' Size Meter
_width = _Newtype <<< prop (SProxy :: SProxy "width")

_height :: Lens' Size Meter
_height = _Newtype <<< prop (SProxy :: SProxy "height")
