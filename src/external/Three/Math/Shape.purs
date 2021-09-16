module Three.Math.Shape where

import Effect (Effect)
import Three.Math.Path (class IsPath)
import Three.Math.Vector (Vector2)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Shape :: Type
foreign import mkShape :: Effect Shape
foreign import mkShapeWith :: Array Vector2 -> Effect Shape

instance IsPath Shape where
    toPath = unsafeCoerce
