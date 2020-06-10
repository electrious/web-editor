module Rendering.Renderable where

import Effect (Effect)
import Three.Core.Object3D (class IsObject3D)

class IsObject3D b <= Renderable a b where
    render :: a -> Effect b
