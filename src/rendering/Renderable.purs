module Rendering.Renderable where

import Effect (Effect)
import Math.Angle (Angle)
import Three.Core.Object3D (class IsObject3D)

class IsObject3D b <= Renderable a b where
    render :: a -> Effect b


class IsObject3D b <= RenderableWithSlope a b where
    renderWithSlope :: a -> Angle -> Effect b
