module Three.Core.WebGLRenderer where

import Prelude

import Effect (Effect)
import Three.Core.Camera (class IsCamera, Camera, toCamera)
import Three.Core.Scene (Scene)
import Web.DOM (Element)

foreign import data WebGLRenderer :: Type

foreign import mkWebGLRenderer :: Effect WebGLRenderer
foreign import toDataUrl :: String -> Element -> Effect String

foreign import setSize :: Int -> Int -> WebGLRenderer -> Effect Unit

foreign import domElement :: WebGLRenderer -> Element

foreign import jsrender :: Scene -> Camera -> WebGLRenderer -> Effect Unit

render :: forall c. IsCamera c => Scene -> c -> WebGLRenderer -> Effect Unit
render s c r = jsrender s (toCamera c) r