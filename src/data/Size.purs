module Data.Hardware.Size where


import Data.Default (class Default)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)

-- | Size type to represent size of real world object in meters
newtype Size = Size {
    width  :: Meter,
    height :: Meter
}

derive instance newtypeSize :: Newtype Size _
instance defaultSize :: Default Size where
    def = Size { width: meter 0.0, height: meter 0.0 }
