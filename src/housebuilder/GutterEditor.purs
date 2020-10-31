module HouseBuilder.GutterEditor where

import Prelude

import Data.Function.Memoize (memoize)
import Effect.Unsafe (unsafePerformEffect)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)

loadMat :: Int -> MeshBasicMaterial
loadMat = memoize (\clr -> unsafePerformEffect $ mkMeshBasicMaterial clr)

greenMat :: MeshBasicMaterial
greenMat = loadMat 0x00ff00

blueMat :: MeshBasicMaterial
blueMat = loadMat 0x0000ff

