module Three.Loader.TextureLoader where

import Effect (Effect)

foreign import data Texture :: Type
foreign import data TextureLoader :: Type

foreign import mkTextureLoader :: Effect TextureLoader
foreign import loadTexture :: String -> TextureLoader -> Effect Texture
