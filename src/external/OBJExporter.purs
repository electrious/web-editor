module OBJExporter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Web.File (File)
import Three.Core.Object3D (Object3D)

foreign import exportObject :: Object3D -> Effect MeshFiles

newtype MeshFiles = MeshFiles {
    obj :: File,
    mtl :: File
    }

derive instance newtypeMeshFiles :: Newtype MeshFiles _
derive instance genericMeshFiles :: Generic MeshFiles _

_obj :: forall t a r. Newtype t { obj :: a | r } => Lens' t a
_obj = _Newtype <<< prop (SProxy :: SProxy "obj")

_mtl :: forall t a r. Newtype t { mtl :: a | r } => Lens' t a
_mtl = _Newtype <<< prop (SProxy :: SProxy "mtl")
