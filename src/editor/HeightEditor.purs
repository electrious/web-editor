module Editor.HeightEditor where

import Prelude

import Data.Default (def)
import Data.Lens (Lens', view, (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_isActive, _position, _rotation)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Math (pi)
import Rendering.Node (Node, getEnv, node)
import Three.Core.Geometry (Geometry)
import Three.Core.Material (MeshBasicMaterial)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (Vector3, mkVec3, vecZ)
import UI.DraggableObject (DragObjCfg, DraggableObject, _customMat, _deltaTransform, _validator, createDraggableObject)

type DragArrow = DraggableObject

newtype DragArrowConf = DragArrowConf {
    arrowMaterial :: Maybe MeshBasicMaterial
    }

derive instance newtypeDragArrowConf :: Newtype DragArrowConf _

_arrowMaterial :: forall t a r. Newtype t { arrowMaterial :: a | r } => Lens' t a
_arrowMaterial = _Newtype <<< prop (SProxy :: SProxy "arrowMaterial")

mkDragArrowConf :: Maybe MeshBasicMaterial -> DragArrowConf
mkDragArrowConf m = DragArrowConf { arrowMaterial : m }


-- | create the drag arrow to drag the Floor Plan to form the house
dragArrow :: Dynamic Boolean -> Dynamic Vector3 -> Node DragArrowConf DragArrow
dragArrow actDyn posDyn = do
    mat <- view _arrowMaterial <$> getEnv
    
    let -- make sure the arrow can only be dragged between 0 and 20 in Z axis
        validator pos = let z = vecZ pos in z >= 0.0 && z <= 20.0
        -- make sure the arrow only can be dragged along Z axis
        transF d = mkVec3 0.0 0.0 (vecZ d)
        
        cfg = def # _isActive       .~ actDyn
                  # _customMat      .~ mat
                  # _validator      .~ validator
                  # _deltaTransform .~ Just transF
                  # _rotation       .~ mkEuler (pi / 2.0) 0.0 0.0
    node (def # _position .~ posDyn) $ createDraggableObject (cfg :: DragObjCfg Geometry)


-- | setup drag arrow to edit the house height
setupHeightEditor :: Dynamic Boolean -> Dynamic Vector3 -> Node DragArrowConf (Event Meter)
setupHeightEditor actDyn posDyn = do
    arrow <- dragArrow actDyn posDyn
    let toH = meter <<< vecZ
    pure $ toH <$> (arrow ^. _position)
