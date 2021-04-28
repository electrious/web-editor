module OBJExporter where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Three.Core.Object3D (Object3D)

foreign import data OBJExporter :: Type

foreign import mkOBJExporter :: Effect OBJExporter
foreign import parseObject :: Object3D -> OBJExporter -> Effect MeshFiles

newtype MeshFiles = MeshFiles {
    obj :: String,
    mtl :: String
    }

derive instance newtypeMeshFiles :: Newtype MeshFiles _

_obj :: forall t a r. Newtype t { obj :: a | r } => Lens' t a
_obj = _Newtype <<< prop (SProxy :: SProxy "obj")

_mtl :: forall t a r. Newtype t { mtl :: a | r } => Lens' t a
_mtl = _Newtype <<< prop (SProxy :: SProxy "mtl")


exportObject :: Object3D -> Effect MeshFiles
exportObject obj = mkOBJExporter >>= parseObject obj
