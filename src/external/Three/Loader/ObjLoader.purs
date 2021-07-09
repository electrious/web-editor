module Three.Loader.ObjLoader where

import Prelude

import Effect (Effect)
import Three.Core.Material (MaterialCreator)
import Three.Core.Object3D (class IsObject3D, Object3D)
import Unsafe.Coerce (unsafeCoerce)
  
foreign import data MTLLoader :: Type 
foreign import data OBJLoader :: Type
  
foreign import makeMTLLoader :: Effect MTLLoader
foreign import makeOBJLoader :: Effect OBJLoader

foreign import setPath :: String -> MTLLoader -> Effect Unit
foreign import loadMTL :: MTLLoader ->  String -> (MaterialCreator -> Effect Unit) -> Effect Unit
foreign import jsloadOBJ :: OBJLoader -> String -> (Object3D -> Effect Unit) -> Effect Unit
foreign import parseOBJ :: String -> OBJLoader -> Object3D
foreign import setMaterials :: MaterialCreator -> OBJLoader -> Effect Unit

loadOBJ :: forall a. IsObject3D a => OBJLoader -> String -> (a -> Effect Unit) -> Effect Unit
loadOBJ l n f = jsloadOBJ l n (unsafeCoerce >>> f)
