module Three.Core.Object3D where

import Prelude

import Effect (Effect)
import Three.Math.Vector (Vector3)
import Util (ffi, fpi)

foreign import data JSObject3D :: Type -> Type
type Object3D = JSObject3D

mkObject3D :: forall a. Effect (Object3D a)
mkObject3D = ffi [""] "new THREE.Object3D()"

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

setRenderOrder :: forall a. Int -> Object3D a -> Effect Unit
setRenderOrder = fpi ["r", "o", ""] "o.renderOrder = r"

setVisible :: forall a. Boolean -> Object3D a -> Effect Unit
setVisible = fpi ["v", "o", ""] "o.visible = v"

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
