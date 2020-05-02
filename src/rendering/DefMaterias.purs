module Editor.Rendering.DefMaterials where

import Prelude

import Data.Function.Memoize (memoize)
import Effect.Unsafe (unsafePerformEffect)
import Model.Hardware.PanelType (PanelType(..))
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
  
-- | memoized function to load material for a panel color type
loadMaterial :: forall a. PanelType -> MeshBasicMaterial a
loadMaterial = memoize (unsafePerformEffect <<< mkMeshBasicMaterial <<< color)
    where color Premium  = 0x191919
          color Standard = 0xbfbfbf
