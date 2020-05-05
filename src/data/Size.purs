module Data.Hardware.Size where


import Data.Meter (Meter)
import Data.Newtype (class Newtype)

-- | Size type to represent size of real world object in meters
newtype Size = Size {
    width  :: Meter,
    height :: Meter
}

derive instance newtypeSize :: Newtype Size _
