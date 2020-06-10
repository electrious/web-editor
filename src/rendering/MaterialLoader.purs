module Renderring.MaterialLoader where

import Prelude

import Data.Function.Memoize (memoize)
import Effect.Unsafe (unsafePerformEffect)
import Model.Hardware.PanelType (PanelType(..))
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)

blackMaterial :: MeshBasicMaterial
blackMaterial = unsafePerformEffect $ mkMeshBasicMaterial 0x191919

silverMaterial :: MeshBasicMaterial
silverMaterial = unsafePerformEffect $ mkMeshBasicMaterial 0xbfbfbf

materialForType :: PanelType -> MeshBasicMaterial
materialForType = memoize f
    where f Premium  = blackMaterial
          f Standard = silverMaterial
