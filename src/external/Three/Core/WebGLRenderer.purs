module Three.Core.WebGLRenderer where

import Prelude

import Effect (Effect)
import Three.Core.Camera (class IsCamera)
import Three.Core.Scene (Scene)
import Util (ffi, fpi)
import Web.DOM (Element)

foreign import data WebGLRenderer :: Type

foreign import mkWebGLRenderer :: Effect WebGLRenderer

setSize :: Int -> Int -> WebGLRenderer -> Effect Unit
setSize = fpi ["w", "h", "r", ""] "r.setSize(w, h)"

domElement :: WebGLRenderer -> Element
domElement = ffi ["r"] "r.domElement"

render :: forall c. IsCamera c => Scene -> c -> WebGLRenderer -> Effect Unit
render = fpi ["scene", "camera", "r", ""] "r.render(scene, camera)"
