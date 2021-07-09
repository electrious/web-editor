module Three.Core.Object3D where

import Prelude

import Effect (Effect)
import Three.Math.Euler (Euler)
import Three.Math.Matrix (Matrix4, invert)
import Three.Math.Vector (Vector3, applyMatrix)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Object3D :: Type

foreign import mkObject3D :: Effect Object3D

foreign import setDefaultUp :: Vector3 -> Effect Unit

class IsObject3D a where
    toObject3D :: a -> Object3D

instance isObject3DObject3D :: IsObject3D Object3D where
    toObject3D = identity

foreign import jsCastShadow :: Object3D -> Boolean
foreign import jsSetCastShadow :: Boolean -> Object3D -> Effect Unit
foreign import jsSetReceiveShadow :: Boolean -> Object3D -> Effect Unit

-- | Whether the object gets rendered into shadow map, default False
castShadow :: forall a. IsObject3D a => a -> Boolean
castShadow = toObject3D >>> jsCastShadow

setCastShadow :: forall a. IsObject3D a => Boolean -> a -> Effect Unit
setCastShadow s o = jsSetCastShadow s (toObject3D o)

setReceiveShadow :: forall a. IsObject3D a => Boolean -> a -> Effect Unit
setReceiveShadow r o = jsSetReceiveShadow r (toObject3D o)

foreign import jsChildren :: Object3D -> Array Object3D

children :: forall a b. IsObject3D a => IsObject3D b => a -> Array b
children = toObject3D >>> jsChildren >>> map unsafeCoerce

foreign import jsHasParent :: Object3D -> Boolean
foreign import jsParent :: Object3D -> Object3D

hasParent :: forall a. IsObject3D a => a -> Boolean
hasParent = toObject3D >>> jsHasParent

parent :: forall a b. IsObject3D a => IsObject3D b => a -> b
parent = toObject3D >>> jsParent >>> unsafeCoerce

foreign import jsAdd :: Object3D -> Object3D -> Effect Unit
foreign import jsRemove :: Object3D -> Object3D -> Effect Unit

add :: forall child parent. IsObject3D child => IsObject3D parent => child -> parent -> Effect Unit
add c p = jsAdd (toObject3D c) (toObject3D p)

remove :: forall child parent. IsObject3D child => IsObject3D parent => child -> parent -> Effect Unit
remove c p = jsRemove (toObject3D c) (toObject3D p)

foreign import jsSetName :: String -> Object3D -> Effect Unit

setName :: forall a. IsObject3D a => String -> a -> Effect Unit
setName n o = jsSetName n (toObject3D o)

foreign import jsPosition :: Object3D -> Vector3
foreign import jsSetPosition :: Vector3 -> Object3D -> Effect Unit

position :: forall a. IsObject3D a => a -> Vector3
position = toObject3D >>> jsPosition

setPosition :: forall a. IsObject3D a => Vector3 -> a -> Effect Unit
setPosition v o = jsSetPosition v (toObject3D o)

foreign import jsSetRotation :: Euler -> Object3D -> Effect Unit
setRotation :: forall a. IsObject3D a => Euler -> a ->Effect Unit
setRotation e o = jsSetRotation e (toObject3D o)

foreign import jsSetScale :: Vector3 -> Object3D -> Effect Unit

setScale :: forall a. IsObject3D a => Vector3 -> a -> Effect Unit
setScale s o = jsSetScale s (toObject3D o)

foreign import jsRotateX :: Number -> Object3D -> Effect Unit
foreign import jsRotateY :: Number -> Object3D -> Effect Unit
foreign import jsRotateZ :: Number -> Object3D -> Effect Unit
foreign import jsRotateOnWorldAxis :: Vector3 -> Number -> Object3D -> Effect Unit
foreign import jsRotateWithEuler :: Euler -> Object3D -> Effect Unit

rotateX :: forall a. IsObject3D a => Number -> a -> Effect Unit
rotateX r o = jsRotateX r (toObject3D o)

rotateY :: forall a. IsObject3D a => Number -> a -> Effect Unit
rotateY r o = jsRotateY r (toObject3D o)

rotateZ :: forall a. IsObject3D a => Number -> a -> Effect Unit
rotateZ r o = jsRotateZ r (toObject3D o)

rotateOnWorldAxis :: forall a. IsObject3D a => Vector3 -> Number -> a -> Effect Unit
rotateOnWorldAxis v d o = jsRotateOnWorldAxis v d (toObject3D o)

rotateWithEuler :: forall a. IsObject3D a => Euler -> a -> Effect Unit
rotateWithEuler e o = jsRotateWithEuler e (toObject3D o)

foreign import jsTranslateX :: Number -> Object3D -> Effect Unit
foreign import jsTranslateY :: Number -> Object3D -> Effect Unit
foreign import jsTranslateZ :: Number -> Object3D -> Effect Unit

translateX :: forall a. IsObject3D a => Number -> a -> Effect Unit
translateX x o = jsTranslateX x (toObject3D o)

translateY :: forall a. IsObject3D a => Number -> a -> Effect Unit
translateY y o = jsTranslateY y (toObject3D o)

translateZ :: forall a. IsObject3D a => Number -> a -> Effect Unit
translateZ z o = jsTranslateZ z (toObject3D o)

foreign import jsSetRenderOrder :: Int -> Object3D -> Effect Unit

setRenderOrder :: forall a. IsObject3D a => Int -> a -> Effect Unit
setRenderOrder r o = jsSetRenderOrder r (toObject3D o)

foreign import jsSetVisible :: Boolean -> Object3D -> Effect Unit

setVisible :: forall a. IsObject3D a => Boolean -> a -> Effect Unit
setVisible v o = jsSetVisible v (toObject3D o)

foreign import jsMatrix :: Object3D -> Matrix4
foreign import jsUpdateMatrix :: Object3D -> Effect Unit
foreign import jsUpdateMatrixWorld :: Object3D -> Effect Unit

matrix :: forall a. IsObject3D a => a -> Matrix4
matrix = toObject3D >>> jsMatrix

updateMatrix :: forall a. IsObject3D a => a -> Effect Unit
updateMatrix = toObject3D >>> jsUpdateMatrix

updateMatrixWorld :: forall a. IsObject3D a => a -> Effect Unit
updateMatrixWorld = toObject3D >>> jsUpdateMatrixWorld

foreign import jsLocalToWorld :: Vector3 -> Object3D -> Effect Vector3
foreign import jsWorldToLocal :: Vector3 -> Object3D -> Effect Vector3

localToWorld :: forall a. IsObject3D a => Vector3 -> a -> Effect Vector3
localToWorld v o = jsLocalToWorld v (toObject3D o)

worldToLocal :: forall a. IsObject3D a => Vector3 -> a -> Effect Vector3
worldToLocal v o = jsWorldToLocal v (toObject3D o)

localToParent :: forall a. IsObject3D a => Vector3 -> a -> Effect Vector3
localToParent v o = pure $ applyMatrix (matrix o) v

parentToLocal :: forall a. IsObject3D a => Vector3 -> a -> Effect Vector3
parentToLocal v o = pure $ applyMatrix (invert $ matrix o) v

foreign import jsLookAt :: Vector3 -> Object3D -> Effect Unit

lookAt :: forall a. IsObject3D a => Vector3 -> a -> Effect Unit
lookAt v o = jsLookAt v (toObject3D o)

foreign import jsclone :: Object3D -> Effect Object3D

clone :: forall a. IsObject3D a => a -> Effect a
clone o = unsafeCoerce <$> jsclone (toObject3D o)

foreign import jsUserData :: Object3D -> String
foreign import jsSetUserData :: String -> Object3D -> Effect Unit

-- | get user data. Make it a String here
userData :: forall a. IsObject3D a => a -> String
userData = toObject3D >>> jsUserData

-- | set a custom user Data string
setUserData :: forall a. IsObject3D a => String -> a -> Effect Unit
setUserData d o = jsSetUserData d (toObject3D o)

foreign import jsEnableLayer :: Int -> Object3D -> Effect Unit
foreign import jsDisableLayer :: Int -> Object3D -> Effect Unit

enableLayer :: forall o. IsObject3D o => Int -> o -> Effect Unit
enableLayer l o = jsEnableLayer l (toObject3D o)

disableLayer :: forall o. IsObject3D o => Int -> o -> Effect Unit
disableLayer l o = jsDisableLayer l (toObject3D o)
