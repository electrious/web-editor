module Three.Math.Color where

import Effect (Effect)

foreign import data Color :: Type

foreign import mkColor :: Int -> Effect Color
foreign import mkColorString :: String -> Effect Color
foreign import mkColorRGB :: Number -> Number -> Number -> Effect Color
