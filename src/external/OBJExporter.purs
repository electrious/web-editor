module OBJExporter where

import Prelude
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Three.Core.Object3D (Object3D)

foreign import data OBJExporter :: Type
foreign import data ExportData :: Type

foreign import mkOBJExporter :: Effect OBJExporter
foreign import parseObject :: Object3D -> OBJExporter -> Effect ExportData

foreign import getOBJ :: ExportData -> String
foreign import getMTL :: ExportData -> String


exportObject :: Object3D -> Effect (Tuple String String)
exportObject obj = do
    e <- mkOBJExporter
    r <- parseObject obj e
    pure $ Tuple (getOBJ r) (getMTL r)
