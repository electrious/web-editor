module Three.Loader.TextureLoader where

import Prelude

import Effect (Effect)

foreign import data Texture  :: Type
foreign import data WrapMode :: Type
                    
foreign import clampToEdgeWrapping    :: WrapMode
foreign import repeatWrapping         :: WrapMode
foreign import mirroredRepeatWrapping :: WrapMode

foreign import setWrapS  :: WrapMode -> Texture -> Effect Unit
foreign import setWrapT  :: WrapMode -> Texture -> Effect Unit
foreign import setRepeat :: Number -> Number -> Texture -> Effect Unit

foreign import data TextureLoader :: Type

foreign import mkTextureLoader :: Effect TextureLoader
foreign import loadTexture :: String -> TextureLoader -> Effect Texture
foreign import loadTextureAsync :: String -> TextureLoader -> (Texture -> Effect Unit) -> Effect Unit
foreign import dispose :: Texture -> Effect Unit
foreign import textureWidth :: Texture -> Int
foreign import textureHeight :: Texture -> Int
