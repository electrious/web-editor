module Three.Core.Material where

import Prelude

import Effect (Effect)
import Three.Loader.TextureLoader (Texture)
import Three.Math.Color (Color)
import Unsafe.Coerce (unsafeCoerce)
  
foreign import data Material :: Type
foreign import data MeshBasicMaterial :: Type
foreign import data MeshPhongMaterial :: Type

foreign import data LineMaterial :: Type
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

class IsMaterial a where
    toMaterial :: a -> Material

instance IsMaterial MeshBasicMaterial where
    toMaterial = unsafeCoerce
instance IsMaterial MeshPhongMaterial where
    toMaterial = unsafeCoerce

class IsLineMaterial a where
    toLineMaterial :: a -> LineMaterial

instance IsLineMaterial LineBasicMaterial where
    toLineMaterial = unsafeCoerce
instance IsLineMaterial LineDashedMaterial where
    toLineMaterial = unsafeCoerce

foreign import jssetTransparent :: Boolean -> Material -> Effect Unit

setTransparent :: forall mat. IsMaterial mat => Boolean -> mat -> Effect Unit
setTransparent t = jssetTransparent t <<< toMaterial

foreign import jssetOpacity :: Number -> Material -> Effect Unit

setOpacity :: forall mat. IsMaterial mat => Number -> mat -> Effect Unit
setOpacity o = jssetOpacity o <<< toMaterial

foreign import jssetDepthWrite :: Boolean -> Material -> Effect Unit

setDepthWrite :: forall mat. IsMaterial mat => Boolean -> mat -> Effect Unit
setDepthWrite d = jssetDepthWrite d <<< toMaterial

foreign import data Side :: Type
foreign import frontSide :: Side
foreign import backSide :: Side
foreign import doubleSide :: Side

foreign import jssetSide :: Side -> Material -> Effect Unit

setSide :: forall mat. IsMaterial mat => Side -> mat -> Effect Unit
setSide s = jssetSide s <<< toMaterial

foreign import data MaterialCreator :: Type

foreign import jsgetMaterial :: String -> MaterialCreator -> Material
foreign import preload :: MaterialCreator -> Effect Unit

getMaterial :: forall mat. IsMaterial mat => String -> MaterialCreator -> mat
getMaterial mat creator = unsafeCoerce $ jsgetMaterial mat creator
