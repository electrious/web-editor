module Three.Core.Object3D where

import Prelude

import Effect (Effect)
import Three.Math.Euler (Euler)
import Three.Math.Matrix (Matrix4)
import Three.Math.Vector (Vector3)
import Util (ffi, fpi)

foreign import data Object3D :: Type

foreign import mkObject3D :: Effect Object3D

foreign import setDefaultUp :: Vector3 -> Effect Unit

class IsObject3D a

instance isObject3DObject3D :: IsObject3D Object3D

-- | Whether the object gets rendered into shadow map, default False
castShadow :: forall a. IsObject3D a => a -> Boolean
castShadow = ffi ["o"] "o.castShadow"

setCastShadow :: forall a. IsObject3D a => Boolean -> a -> Effect Unit
setCastShadow = fpi ["s", "o", ""] "o.castShadow = s"

setReceiveShadow :: forall a. IsObject3D a => Boolean -> a -> Effect Unit
setReceiveShadow = fpi ["s", "o", ""] "o.receiveShadow = s"

children :: forall a b. IsObject3D a => IsObject3D b => a -> Array b
children = ffi ["o"] "o.children"

hasParent :: forall a. IsObject3D a => a -> Boolean
hasParent = ffi ["o"] "o.parent !== null && o.parent !== undefined"

parent :: forall a b. IsObject3D a => IsObject3D b => a -> b
parent = ffi ["o"] "o.parent"

add :: forall a b. IsObject3D a => IsObject3D b => a -> b -> Effect Unit
add = fpi ["child", "parent", ""] "parent.add(child)"

remove :: forall a b. IsObject3D a => IsObject3D b => a -> b -> Effect Unit
remove = fpi ["child", "parent", ""] "parent.remove(child)"

setName :: forall a. IsObject3D a => String -> a -> Effect Unit
setName = fpi ["name", "obj", ""] "obj.name = name"

position :: forall a. IsObject3D a => a -> Vector3
position = ffi ["o"] "o.position"

setPosition :: forall a. IsObject3D a => Vector3 -> a -> Effect Unit
setPosition = fpi ["v", "o", ""] "o.position.copy(v)"

setRotation :: forall a. IsObject3D a => Euler -> a ->Effect Unit
setRotation = fpi ["e", "o", ""] "o.rotation.copy(e)"

setScale :: forall a. IsObject3D a => Vector3 -> a -> Effect Unit
setScale = fpi ["s", "o", ""] "o.scale.copy(s)"

rotateX :: forall a. IsObject3D a => Number -> a -> Effect Unit
rotateX = fpi ["r", "o", ""] "o.rotateX(r)"

rotateY :: forall a. IsObject3D a => Number -> a -> Effect Unit
rotateY = fpi ["r", "o", ""] "o.rotateY(r)"

rotateZ :: forall a. IsObject3D a => Number -> a -> Effect Unit
rotateZ = fpi ["r", "o", ""] "o.rotateZ(r)"

rotateOnWorldAxis :: forall a. IsObject3D a => Vector3 -> Number -> a -> Effect Unit
rotateOnWorldAxis = fpi ["v", "d", "o", ""] "o.rotateOnWorldAxis(v, d)"

rotateWithEuler :: forall a. IsObject3D a => Euler -> a -> Effect Unit
rotateWithEuler = fpi ["e", "o", ""] "o.setRotationFromEuler(e)"

translateX :: forall a. IsObject3D a => Number -> a -> Effect Unit
translateX = fpi ["x", "o", ""] "o.translateX(x)"

translateY :: forall a. IsObject3D a => Number -> a -> Effect Unit
translateY = fpi ["y", "o", ""] "o.translateY(y)"

translateZ :: forall a. IsObject3D a => Number -> a -> Effect Unit
translateZ = fpi ["z", "o", ""] "o.translateZ(z)"

setRenderOrder :: forall a. IsObject3D a => Int -> a -> Effect Unit
setRenderOrder = fpi ["r", "o", ""] "o.renderOrder = r"

setVisible :: forall a. IsObject3D a => Boolean -> a -> Effect Unit
setVisible = fpi ["v", "o", ""] "o.visible = v"

matrix :: forall a. IsObject3D a => a -> Matrix4
matrix = ffi ["o"] "o.matrix"

updateMatrix :: forall a. IsObject3D a => a -> Effect Unit
updateMatrix = fpi ["o", ""] "o.updateMatrix()"

updateMatrixWorld :: forall a. IsObject3D a => a -> Effect Unit
updateMatrixWorld = fpi ["o", ""] "o.updateMatrixWorld()"

localToWorld :: forall a. IsObject3D a => Vector3 -> a -> Effect Vector3
localToWorld = ffi ["v", "o", ""] "o.localToWorld(v.clone())"

worldToLocal :: forall a. IsObject3D a => Vector3 -> a -> Effect Vector3
worldToLocal = ffi ["v", "o", ""] "o.worldToLocal(v.clone())"

lookAt :: forall a. IsObject3D a => Vector3 -> a -> Effect Unit
lookAt = fpi ["v", "o", ""] "o.lookAt(v)"

clone :: forall a. IsObject3D a => a -> Effect a
clone = ffi ["o", ""] "o.clone()"

-- | get user data. Make it a String here
userData :: forall a. IsObject3D a => a -> String
userData = ffi ["o"] "o.userData"

-- | set a custom user Data string
setUserData :: forall a. IsObject3D a => String -> a -> Effect Unit
setUserData = fpi ["d", "o", ""] "o.userData = d"
