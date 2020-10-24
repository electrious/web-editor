module Rendering.TextureLoader where

import Prelude

import Data.Function.Memoize (memoize)
import Effect.Unsafe (unsafePerformEffect)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterialWithTexture)
import Three.Loader.TextureLoader (Texture, loadTexture, mkTextureLoader)

loadTextureFromUrl :: String -> Texture
loadTextureFromUrl = memoize \imgPath -> unsafePerformEffect do
    loader <- mkTextureLoader
    loadTexture imgPath loader

loadMaterialFromUrl :: String -> MeshBasicMaterial
loadMaterialFromUrl = memoize \url -> unsafePerformEffect do
    loader <- mkTextureLoader
    t <- loadTexture url loader
    mkMeshBasicMaterialWithTexture t
