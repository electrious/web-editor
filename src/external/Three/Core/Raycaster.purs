module Three.Core.Raycaster where

import Prelude

import Effect (Effect)
import Three.Core.Camera (class IsCamera, Camera, toCamera)
import Three.Core.Face3 (Face3)
import Three.Core.Object3D (class IsObject3D, Object3D, toObject3D)
import Three.Math.Vector (Vector2, Vector3)

foreign import data Raycaster :: Type
foreign import data Intersection :: Type

foreign import mkRaycaster :: Effect Raycaster

-- | Updates the ray with a new origin and direction.
foreign import jssetFromCamera :: Raycaster -> Vector2 -> Camera -> Effect Unit

setFromCamera :: forall c. IsCamera c => Raycaster -> Vector2 -> c -> Effect Unit
setFromCamera rc v = jssetFromCamera rc v <<< toCamera

foreign import jsintersectObject :: Raycaster -> Object3D -> Boolean -> Effect (Array Intersection)

intersectObject :: forall a. IsObject3D a => Raycaster -> a -> Boolean -> Effect (Array Intersection)
intersectObject rc obj r = jsintersectObject rc (toObject3D obj) r

-- | distance between the origin of the ray and the intersection
foreign import distance :: Intersection -> Number

-- | point of intersection, in world coordinates
foreign import point :: Intersection -> Vector3

-- | intersected face
foreign import face :: Intersection -> Face3

-- | the intersected object
foreign import object :: Intersection -> Object3D
