module HouseBuilder.Rendering.Materials where

import Prelude

import Data.Function.Memoize (memoize)
import Effect.Unsafe (unsafePerformEffect)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)

-- materials used in gutter editor
loadMat :: Int -> MeshBasicMaterial
loadMat = memoize (unsafePerformEffect <<< mkMeshBasicMaterial)

greenMat :: MeshBasicMaterial
greenMat = loadMat 0x00ff00

blueMat :: MeshBasicMaterial
blueMat = loadMat 0x0000ff

redMat :: MeshBasicMaterial
redMat = loadMat 0xff0000
