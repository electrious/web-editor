module Three.Core.Material where

import Prelude

import Effect (Effect)
import Three.Loader.TextureLoader (Texture)
import Three.Math.Color (Color)
  
foreign import data Material :: Type
foreign import data MeshBasicMaterial :: Type
foreign import data MeshPhongMaterial :: Type
foreign import data LineBasicMaterial :: Type
foreign import data LineDashedMaterial :: Type

foreign import mkMeshBasicMaterial :: Int -> Effect MeshBasicMaterial
foreign import mkMeshBasicMaterialWithColor :: Color -> Effect MeshBasicMaterial
foreign import mkMeshBasicMaterialWithTexture :: Texture -> Effect MeshBasicMaterial
foreign import mkMeshPhongMaterial :: Int -> Effect MeshPhongMaterial

-- create LineBasicMaterial with color and line width
foreign import mkLineBasicMaterial :: Int -> Number -> Effect LineBasicMaterial
-- create LineDashedMaterial with color, line width, scale, dash size and gap size
foreign import mkLineDashedMaterial :: Int -> Number -> Number -> Number -> Number -> Effect LineDashedMaterial

class IsMaterial :: forall k. k -> Constraint
class IsMaterial a

instance isMaterialMaterial :: IsMaterial Material
instance isMaterialMeshBasicMaterial :: IsMaterial MeshBasicMaterial
instance isMaterialMeshPhongMaterial :: IsMaterial MeshPhongMaterial

class IsLineMaterial :: forall k. k -> Constraint
class IsLineMaterial a

instance isLineMatLineBasicMaterial :: IsLineMaterial LineBasicMaterial
instance isLineMatLineDashedMaterial :: IsLineMaterial LineDashedMaterial

foreign import setTransparent :: forall mat. IsMaterial mat => Boolean -> mat -> Effect Unit

foreign import setOpacity :: forall mat. IsMaterial mat => Number -> mat -> Effect Unit

foreign import setDepthWrite :: forall mat. IsMaterial mat => Boolean -> mat -> Effect Unit

foreign import data Side :: Type
foreign import frontSide :: Side
foreign import backSide :: Side
foreign import doubleSide :: Side

foreign import setSide :: forall mat. IsMaterial mat => Side -> mat -> Effect Unit

foreign import data MaterialCreator :: Type

foreign import getMaterial :: forall mat. IsMaterial mat => String -> MaterialCreator -> mat
foreign import preload :: MaterialCreator -> Effect Unit
