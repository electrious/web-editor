module Three.Loader.ObjLoader where

import Prelude

import Effect (Effect)
import Three.Core.Material (MaterialCreator)
import Three.Core.Object3D (Object3D)
import Util (fpi)
  
foreign import data MTLLoader :: Type 
foreign import data OBJLoader2 :: Type

foreign import makeMTLLoader :: Effect MTLLoader
foreign import makeOBJLoader2 :: Effect OBJLoader2

setPath :: String -> MTLLoader -> Effect Unit
setPath = fpi ["path", "loader", ""] "loader.setPath(path)"

loadMTL :: MTLLoader ->  String -> (MaterialCreator -> Effect Unit) -> Effect Unit
loadMTL = fpi ["loader", "name", "cb", ""] "loader.load(name, function(mat) { cb(mat)() })"

loadOBJ :: forall a. OBJLoader2 -> String -> (Object3D a -> Effect Unit) -> Effect Unit
loadOBJ = fpi ["loader", "name", "cb", ""] "loader.load(name, function(obj) { cb(obj)() })"
