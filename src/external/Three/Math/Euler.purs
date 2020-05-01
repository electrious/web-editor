module Three.Math.Euler where

foreign import data Euler :: Type

foreign import mkEuler :: Number -> Number -> Number -> Euler
foreign import clone :: Euler -> Euler
foreign import equal :: Euler -> Euler -> Boolean
