module Three.Core.Object3D where

import Prelude

import Effect (Effect)
import Three.Math.Matrix (Matrix4)
import Three.Math.Vector (Vector3)
import Util (ffi, fpi)

foreign import data JSObject3D :: Type -> Type
type Object3D = JSObject3D

foreign import mkObject3D :: forall a. Effect (Object3D a)

-- | Whether the object gets rendered into shadow map, default False
castShadow :: forall a. Object3D a -> Boolean
castShadow = ffi ["o"] "o.castShadow"

setCastShadow :: forall a. Boolean -> Object3D a -> Effect Unit
setCastShadow = fpi ["s", "o", ""] "o.castShadow = s"

setReceiveShadow :: forall a. Boolean -> Object3D a -> Effect Unit
setReceiveShadow = fpi ["s", "o", ""] "o.receiveShadow = s"

children :: forall a b. Object3D a -> Array (Object3D b)
children = ffi ["o"] "o.children"

hasParent :: forall a. Object3D a -> Boolean
hasParent = ffi ["o"] "o.parent !== null && o.parent !== undefined"

parent :: forall a b. Object3D a -> Object3D b
parent = ffi ["o"] "o.parent"

add :: forall a b. Object3D a -> Object3D b -> Effect Unit
add = fpi ["child", "parent", ""] "parent.add(child)"

remove :: forall a b. Object3D a -> Object3D b -> Effect Unit
remove = fpi ["child", "parent", ""] "parent.remove(child)"

setName :: forall a. String -> Object3D a -> Effect Unit
setName = fpi ["name", "obj", ""] "obj.name = name"

position :: forall a. Object3D a -> Vector3
position = ffi ["o"] "o.position"

setPosition :: forall a. Vector3 -> Object3D a -> Effect Unit
setPosition = fpi ["v", "o", ""] "o.position.copy(v)"

rotateX :: forall a. Number -> Object3D a -> Effect Unit
rotateX = fpi ["r", "o", ""] "o.rotateX(r)"

rotateY :: forall a. Number -> Object3D a -> Effect Unit
rotateY = fpi ["r", "o", ""] "o.rotateY(r)"

rotateZ :: forall a. Number -> Object3D a -> Effect Unit
rotateZ = fpi ["r", "o", ""] "o.rotateZ(r)"

rotateOnWorldAxis :: forall a. Vector3 -> Number -> Object3D a -> Effect Unit
rotateOnWorldAxis = fpi ["v", "d", "o", ""] "o.rotateOnWorldAxis(v, d)"

translateX :: forall a. Number -> Object3D a -> Effect Unit
translateX = fpi ["x", "o", ""] "o.translateX(x)"

translateY :: forall a. Number -> Object3D a -> Effect Unit
translateY = fpi ["y", "o", ""] "o.translateY(y)"

translateZ :: forall a. Number -> Object3D a -> Effect Unit
translateZ = fpi ["z", "o", ""] "o.translateZ(z)"

setRenderOrder :: forall a. Int -> Object3D a -> Effect Unit
setRenderOrder = fpi ["r", "o", ""] "o.renderOrder = r"

setVisible :: forall a. Boolean -> Object3D a -> Effect Unit
setVisible = fpi ["v", "o", ""] "o.visible = v"

matrix :: forall a. Object3D a -> Matrix4
matrix = ffi ["o"] "o.matrix"

updateMatrix :: forall a. Object3D a -> Effect Unit
updateMatrix = fpi ["o", ""] "o.updateMatrix()"

updateMatrixWorld :: forall a. Object3D a -> Effect Unit
updateMatrixWorld = fpi ["o", ""] "o.updateMatrixWorld()"

localToWorld :: forall a. Vector3 -> Object3D a -> Effect Vector3
localToWorld = ffi ["v", "o", ""] "o.localToWorld(v.clone())"

worldToLocal :: forall a. Vector3 -> Object3D a -> Effect Vector3
worldToLocal = ffi ["v", "o", ""] "o.worldToLocal(v.clone())"

lookAt :: forall a. Vector3 -> Object3D a -> Effect Unit
lookAt = fpi ["v", "o", ""] "o.lookAt(v)"

clone :: forall a. Object3D a -> Effect (Object3D a)
clone = ffi ["o", ""] "o.clone()"

-- | get user data. Make it a String here
userData :: forall a. Object3D a -> String
userData = ffi ["o"] "o.userData"

-- | set a custom user Data string
setUserData :: forall a. String -> Object3D a -> Effect Unit
setUserData = fpi ["d", "o", ""] "o.userData = d"