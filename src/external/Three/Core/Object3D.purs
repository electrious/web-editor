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

class IsObject3D a where
    toObject3D :: a -> Object3D

instance isObject3DObject3D :: IsObject3D Object3D where
    toObject3D = identity

-- | Whether the object gets rendered into shadow map, default False
castShadow :: forall a. IsObject3D a => a -> Boolean
castShadow = toObject3D >>> ffi ["o"] "o.castShadow"

setCastShadow :: forall a. IsObject3D a => Boolean -> a -> Effect Unit
setCastShadow s o = let f = fpi ["s", "o", ""] "o.castShadow = s"
                    in f s (toObject3D o)

setReceiveShadow :: forall a. IsObject3D a => Boolean -> a -> Effect Unit
setReceiveShadow r o = let f = fpi ["s", "o", ""] "o.receiveShadow = s"
                       in f r (toObject3D o)

children :: forall a b. IsObject3D a => IsObject3D b => a -> Array b
children = toObject3D >>> ffi ["o"] "o.children"

hasParent :: forall a. IsObject3D a => a -> Boolean
hasParent = toObject3D >>> ffi ["o"] "o.parent !== null && o.parent !== undefined"

parent :: forall a b. IsObject3D a => IsObject3D b => a -> b
parent = toObject3D >>> ffi ["o"] "o.parent"

add :: forall a b. IsObject3D a => IsObject3D b => a -> b -> Effect Unit
add c p = let f = fpi ["child", "parent", ""] "parent.add(child)"
          in f (toObject3D c) (toObject3D p)

remove :: forall a b. IsObject3D a => IsObject3D b => a -> b -> Effect Unit
remove c p = let f = fpi ["child", "parent", ""] "parent.remove(child)"
             in f (toObject3D c) (toObject3D p)

setName :: forall a. IsObject3D a => String -> a -> Effect Unit
setName n o = let f = fpi ["name", "obj", ""] "obj.name = name"
              in f n (toObject3D o)

position :: forall a. IsObject3D a => a -> Vector3
position = toObject3D >>> ffi ["o"] "o.position"

setPosition :: forall a. IsObject3D a => Vector3 -> a -> Effect Unit
setPosition v o = let f = fpi ["v", "o", ""] "o.position.copy(v)"
                  in f v (toObject3D o)

setRotation :: forall a. IsObject3D a => Euler -> a ->Effect Unit
setRotation e o = let f = fpi ["e", "o", ""] "o.rotation.copy(e)"
                  in f e (toObject3D o)

setScale :: forall a. IsObject3D a => Vector3 -> a -> Effect Unit
setScale s o = let f = fpi ["s", "o", ""] "o.scale.copy(s)"
               in f s (toObject3D o)

rotateX :: forall a. IsObject3D a => Number -> a -> Effect Unit
rotateX r o = let f = fpi ["r", "o", ""] "o.rotateX(r)"
              in f r (toObject3D o)

rotateY :: forall a. IsObject3D a => Number -> a -> Effect Unit
rotateY r o = let f = fpi ["r", "o", ""] "o.rotateY(r)"
              in f r (toObject3D o)

rotateZ :: forall a. IsObject3D a => Number -> a -> Effect Unit
rotateZ r o = let f = fpi ["r", "o", ""] "o.rotateZ(r)"
              in f r (toObject3D o)

rotateOnWorldAxis :: forall a. IsObject3D a => Vector3 -> Number -> a -> Effect Unit
rotateOnWorldAxis v d o = let f = fpi ["v", "d", "o", ""] "o.rotateOnWorldAxis(v, d)"
                          in f v d (toObject3D o)

rotateWithEuler :: forall a. IsObject3D a => Euler -> a -> Effect Unit
rotateWithEuler e o = let f = fpi ["e", "o", ""] "o.setRotationFromEuler(e)"
                      in f e (toObject3D o)

translateX :: forall a. IsObject3D a => Number -> a -> Effect Unit
translateX x o = let f = fpi ["x", "o", ""] "o.translateX(x)"
                 in f x (toObject3D o)

translateY :: forall a. IsObject3D a => Number -> a -> Effect Unit
translateY y o = let f = fpi ["y", "o", ""] "o.translateY(y)"
                 in f y (toObject3D o)

translateZ :: forall a. IsObject3D a => Number -> a -> Effect Unit
translateZ z o = let f = fpi ["z", "o", ""] "o.translateZ(z)"
                 in f z (toObject3D o)

setRenderOrder :: forall a. IsObject3D a => Int -> a -> Effect Unit
setRenderOrder r o = let f = fpi ["r", "o", ""] "o.renderOrder = r"
                     in f r (toObject3D o)

setVisible :: forall a. IsObject3D a => Boolean -> a -> Effect Unit
setVisible v o = let f = fpi ["v", "o", ""] "o.visible = v"
                 in f v (toObject3D o)

matrix :: forall a. IsObject3D a => a -> Matrix4
matrix = toObject3D >>> ffi ["o"] "o.matrix"

updateMatrix :: forall a. IsObject3D a => a -> Effect Unit
updateMatrix = toObject3D >>> fpi ["o", ""] "o.updateMatrix()"

updateMatrixWorld :: forall a. IsObject3D a => a -> Effect Unit
updateMatrixWorld = toObject3D >>> fpi ["o", ""] "o.updateMatrixWorld()"

localToWorld :: forall a. IsObject3D a => Vector3 -> a -> Effect Vector3
localToWorld v o = let f = ffi ["v", "o", ""] "o.localToWorld(v.clone())"
                   in f v (toObject3D o)

worldToLocal :: forall a. IsObject3D a => Vector3 -> a -> Effect Vector3
worldToLocal v o = let f = ffi ["v", "o", ""] "o.worldToLocal(v.clone())"
                   in f v (toObject3D o)

lookAt :: forall a. IsObject3D a => Vector3 -> a -> Effect Unit
lookAt v o = let f = fpi ["v", "o", ""] "o.lookAt(v)"
             in f v (toObject3D o)

clone :: forall a. IsObject3D a => a -> Effect a
clone = toObject3D >>> ffi ["o", ""] "o.clone()"

-- | get user data. Make it a String here
userData :: forall a. IsObject3D a => a -> String
userData = toObject3D >>> ffi ["o"] "o.userData"

-- | set a custom user Data string
setUserData :: forall a. IsObject3D a => String -> a -> Effect Unit
setUserData d o = let f = fpi ["d", "o", ""] "o.userData = d"
                  in f d (toObject3D o)
