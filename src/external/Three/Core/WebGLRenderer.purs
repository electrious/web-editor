module Three.Core.WebGLRenderer where

import Prelude

import Effect (Effect)
import Three.Core.Camera (Camera)
import Three.Core.Scene (Scene)
import Util (ffi, fpi)
import Web.DOM (Element)

foreign import data WebGLRenderer :: Type

foreign import mkWebGLRenderer :: Effect WebGLRenderer
foreign import toDataUrl :: String -> Element -> Effect String

setSize :: Int -> Int -> WebGLRenderer -> Effect Unit
setSize = fpi ["w", "h", "r", ""] "r.setSize(w, h)"

domElement :: WebGLRenderer -> Element
domElement = ffi ["r"] "r.domElement"

render :: forall a b. Scene a -> Camera b -> WebGLRenderer -> Effect Unit
render = fpi ["scene", "camera", "r", ""] "r.render(scene, camera)"
