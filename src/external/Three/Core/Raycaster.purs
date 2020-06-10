module Three.Core.Raycaster where

import Prelude

import Effect (Effect)
import Three.Core.Camera (class IsCamera)
import Three.Core.Face3 (Face3)
import Three.Core.Object3D (class IsObject3D, Object3D)
import Three.Math.Vector (Vector2, Vector3)
import Util (fpi, ffi)

foreign import data Raycaster :: Type
foreign import data Intersection :: Type

foreign import mkRaycaster :: Effect Raycaster

-- | Updates the ray with a new origin and direction.
setFromCamera :: forall c. IsCamera c => Raycaster -> Vector2 -> c -> Effect Unit
setFromCamera = fpi ["r", "coords", "camera", ""] "r.setFromCamera(coords, camera)"

intersectObject :: forall a. IsObject3D a => Raycaster -> a -> Boolean -> Effect (Array Intersection)
intersectObject = ffi ["r", "obj", "rec", ""] "r.intersectObject(obj, rec)"

-- | distance between the origin of the ray and the intersection
distance :: Intersection -> Number
distance = ffi ["obj"] "obj.distance"

-- | point of intersection, in world coordinates
point :: Intersection -> Vector3
point = ffi ["obj"] "obj.point"

-- | intersected face
face :: Intersection -> Face3
face = ffi ["obj"] "obj.face"

-- | the intersected object
object :: Intersection -> Object3D
object = ffi ["obj"] "obj.object"
