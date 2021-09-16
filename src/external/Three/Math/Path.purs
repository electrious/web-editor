module Three.Math.Path where

import Prelude

import Effect (Effect)

foreign import data Path :: Type

foreign import mkPath :: Effect Path
foreign import jsMoveTo :: Number -> Number -> Path -> Effect Unit
foreign import jsLineTo :: Number -> Number -> Path -> Effect Unit
foreign import jsBezierCurveTo :: Number -> Number -> Number -> Number -> Number -> Number -> Path -> Effect Unit


class IsPath p where
    toPath :: p -> Path


moveTo :: forall p. IsPath p => Number -> Number -> p -> Effect Unit
moveTo x y p = jsMoveTo x y (toPath p)

lineTo :: forall p. IsPath p => Number -> Number -> p -> Effect Unit
lineTo x y p = jsLineTo x y (toPath p)

bezierCurveTo :: forall p. IsPath p => Number -> Number -> Number -> Number -> Number -> Number -> p -> Effect Unit
bezierCurveTo x1 y1 x2 y2 x y p = jsBezierCurveTo x1 y1 x2 y2 x y (toPath p)
