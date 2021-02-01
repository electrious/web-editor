module Editor.HeightEditor where

import Prelude

import Data.Default (class Default, def)
import Data.Foldable (class Foldable, foldl)
import Data.Lens (Lens', view, (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_isActive, _position, _rotation)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Math (pi)
import Model.ActiveMode (ActiveMode, isActive)
import Rendering.Node (Node, getEnv, node)
import Three.Core.Geometry (Geometry)
import Three.Core.Material (MeshBasicMaterial)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (class Vector, Vector3, mkVec3, vecX, vecY, vecZ)
import UI.DraggableObject (DragObjCfg, DraggableObject, _customMat, _deltaTransform, _validator, createDraggableObject)

type DragArrow = DraggableObject

newtype DragArrowConf = DragArrowConf {
    arrowMaterial :: Maybe MeshBasicMaterial
    }

derive instance newtypeDragArrowConf :: Newtype DragArrowConf _
instance defaultDragArrowConf :: Default DragArrowConf where
    def = DragArrowConf { arrowMaterial : Nothing }

_arrowMaterial :: forall t a r. Newtype t { arrowMaterial :: a | r } => Lens' t a
_arrowMaterial = _Newtype <<< prop (SProxy :: SProxy "arrowMaterial")

mkDragArrowConf :: Maybe MeshBasicMaterial -> DragArrowConf
mkDragArrowConf m = DragArrowConf { arrowMaterial : m }


dragArrowPos :: forall f v. Foldable f => Vector v => f v -> Vector3
dragArrowPos vs = mkVec3 x y 0.5
    where f Nothing v   = Just v
          f (Just ov) v = if vecX v > vecX ov then Just v else Just ov

          mv = foldl f Nothing vs

          x = maybe 2.0 (((+) 2.0) <<< vecX) mv
          y = maybe 0.0 vecY mv

-- | create the drag arrow to drag the Floor Plan to form the house
dragArrow :: Dynamic ActiveMode -> Dynamic Vector3 -> Node DragArrowConf DragArrow
dragArrow actDyn posDyn = do
    mat <- view _arrowMaterial <$> getEnv
    
    let -- make sure the arrow can only be dragged between 0 and 20 in Z axis
        validator pos = let z = vecZ pos in z >= 0.0 && z <= 20.0
        -- make sure the arrow only can be dragged along Z axis
        transF d = mkVec3 0.0 0.0 (vecZ d)
        
        cfg = def # _isActive       .~ (isActive <$> actDyn)
                  # _customMat      .~ mat
                  # _validator      .~ validator
                  # _deltaTransform .~ Just transF
                  # _rotation       .~ mkEuler (pi / 2.0) 0.0 0.0
    node (def # _position .~ posDyn) $ createDraggableObject (cfg :: DragObjCfg Geometry)


-- | setup drag arrow to edit the house height
setupHeightEditor :: Dynamic ActiveMode -> Dynamic Vector3 -> Node DragArrowConf (Event Meter)
setupHeightEditor actDyn posDyn = do
    arrow <- dragArrow actDyn posDyn
    let toH = meter <<< vecZ
    pure $ toH <$> (arrow ^. _position)
