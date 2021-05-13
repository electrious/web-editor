module Rendering.TextureLoader where

import Prelude

import Data.Function.Memoize (memoize)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, makeEvent)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterialWithTexture)
import Three.Loader.TextureLoader (Texture, loadTexture, loadTextureAsync, mkTextureLoader)

loadTextureFromUrl :: String -> Texture
loadTextureFromUrl = memoize \imgPath -> unsafePerformEffect do
    loader <- mkTextureLoader
    loadTexture imgPath loader

loadMaterialFromUrl :: String -> MeshBasicMaterial
loadMaterialFromUrl = memoize \url -> unsafePerformEffect do
    loader <- mkTextureLoader
    t <- loadTexture url loader
    mkMeshBasicMaterialWithTexture t

-- async texture loading
textureFromUrl :: String -> Event Texture
textureFromUrl url = makeEvent \k -> do
    loader <- mkTextureLoader
    loadTextureAsync url loader k
    pure (pure unit)
