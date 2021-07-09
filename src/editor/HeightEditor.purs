module Editor.HeightEditor where

import Prelude

import Data.Default (class Default, def)
import Data.Foldable (class Foldable, foldl)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Meter (Meter, feetInchStr, meter)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Editor.Common.Lenses (_height, _isActive, _modeDyn, _position, _rotation)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Math (pi)
import Model.ActiveMode (ActiveMode(..), isActive)
import Rendering.Node (Node, _fontSize, _textAlign, _visible, dynText3D, fixNodeDWith, node)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (class Vector, Vector3, mkVec3, vecX, vecY, vecZ)
import UI.DraggableObject (DraggableObject, _customGeo, _customMat, _deltaTransform, _validator, createDraggableObjectWith)

type DragArrow = DraggableObject

newtype HeightEditorConf = HeightEditorConf {
    modeDyn       :: Dynamic ActiveMode,
    position      :: Vector3,
    height        :: Meter,
    min           :: Number,
    max           :: Number,
    arrowMaterial :: Maybe MeshBasicMaterial
    }

derive instance newtypeHeightEditorConf :: Newtype HeightEditorConf _
instance defaultHeightEditorConf :: Default HeightEditorConf where
    def = HeightEditorConf {
        modeDyn       : pure Inactive,
        position      : def,
        height        : def,
        min           : 0.0,
        max           : 20.0,
        arrowMaterial : Nothing
        }

_min :: forall t a r. Newtype t { min :: a | r } => Lens' t a
_min = _Newtype <<< prop (Proxy :: Proxy "min")

_max :: forall t a r. Newtype t { max :: a | r } => Lens' t a
_max = _Newtype <<< prop (Proxy :: Proxy "max")

_arrowMaterial :: forall t a r. Newtype t { arrowMaterial :: a | r } => Lens' t a
_arrowMaterial = _Newtype <<< prop (Proxy :: Proxy "arrowMaterial")

dragArrowPos :: forall f v. Foldable f => Vector v => f v -> Vector3
dragArrowPos vs = mkVec3 x y 0.5
    where f Nothing v   = Just v
          f (Just ov) v = if vecX v > vecX ov then Just v else Just ov

          mv = foldl f Nothing vs

          x = maybe 2.0 (((+) 2.0) <<< vecX) mv
          y = maybe 0.0 vecY mv


arrowGeo :: CircleGeometry
arrowGeo = unsafePerformEffect $ mkCircleGeometry 1.0 30

-- | setup drag arrow to edit the house height
setupHeightEditor :: forall e. HeightEditorConf -> Node e (Event Meter)
setupHeightEditor conf = fixNodeDWith (conf ^. _height) \hDyn -> do
    let actDyn = isActive <$> conf ^. _modeDyn
        pos    = conf ^. _position
        h      = conf ^. _height
        min    = conf ^. _min
        max    = conf ^. _max
        mat    = conf ^. _arrowMaterial

        -- make sure the arrow can only be dragged between 0 and 20 in Z axis
        validator p = let z = vecZ p in z >= min && z <= max
        -- make sure the arrow only can be dragged along Z axis
        transF d = mkVec3 0.0 0.0 (vecZ d)
        
        cfg = def # _isActive       .~ actDyn
                  # _customGeo      .~ Just arrowGeo
                  # _customMat      .~ mat
                  # _validator      .~ pure validator
                  # _deltaTransform .~ Just transF
                  # _rotation       .~ mkEuler (pi / 2.0) 0.0 0.0
                
        hStrDyn = feetInchStr <$> hDyn

        tWrpCfg = def # _position .~ pure (mkVec3 (-0.7) 0.3 0.0)
        tProp = def # _fontSize .~ 0.6
                    # _textAlign .~ "center"

    arrow <- node (def # _position .~ pure pos
                       # _visible  .~ actDyn) $ createDraggableObjectWith cfg (void $ node tWrpCfg $ dynText3D tProp hStrDyn)
    
    let hEvt = (+) h <<< meter <<< vecZ <$> arrow ^. _position
    
    pure { input: hEvt, output: hEvt }
