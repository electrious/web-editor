module Three.Math.Euler where

import Data.Default (class Default)

foreign import data Euler :: Type

instance defaultEuler :: Default Euler where
    def = mkEuler 0.0 0.0 0.0

foreign import mkEuler :: Number -> Number -> Number -> Euler
foreign import clone :: Euler -> Euler
foreign import equal :: Euler -> Euler -> Boolean
