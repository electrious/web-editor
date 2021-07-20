module SmartHouse.SlopeEditor where

import Prelude hiding (degree)

import Data.Default (class Default, def)
import Data.Int (round)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_isActive, _modeDyn, _position, _rotation, _slope)
import Editor.HeightEditor (_arrowMaterial, _max, _min)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic)
import FRP.Event (Event)
import Math (pi)
import Math.Angle (Angle, atan2, degree, degreeVal, tan)
import Model.ActiveMode (ActiveMode(..), isActive)
import Rendering.Line (moveUp)
import Rendering.Node (Node, _fontSize, _textAlign, _visible, dynText3D, fixNodeDWith, node)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (Vector3, mkVec3, vecZ)
import Type.Proxy (Proxy(..))
import UI.DraggableObject (_customGeo, _customMat, _deltaTransform, _validator, createDraggableObjectWith)


-- slope editor config
newtype SlopeEditorConfig = SlopeEditorConfig {
    modeDyn         :: Dynamic ActiveMode,
    position        :: Vector3,
    height          :: Dynamic Number,
    slope           :: Angle,
    maxHeightToEdge :: Number,
    min             :: Angle,
    max             :: Angle,
    arrowMaterial   :: Maybe MeshBasicMaterial
}

derive instance Newtype SlopeEditorConfig _
instance Default SlopeEditorConfig where
    def = SlopeEditorConfig {
        modeDyn         : pure Inactive,
        position        : def,
        height          : pure 0.0,
        slope           : def,
        maxHeightToEdge : 0.0,
        min             : degree 5.0,
        max             : degree 85.0,
        arrowMaterial   : Nothing
    }

_maxHeightToEdge :: forall t a r. Newtype t { maxHeightToEdge :: a | r } => Lens' t a
_maxHeightToEdge = _Newtype <<< prop (Proxy :: Proxy "maxHeightToEdge")

arrowGeo :: CircleGeometry
arrowGeo = unsafePerformEffect $ mkCircleGeometry 1.0 30

showSlope :: Angle -> String
showSlope slope = "Slope: \n" <> show (round $ degreeVal slope) <> "°"

slopeToHeight :: SlopeEditorConfig -> Angle -> Number
slopeToHeight cfg slope = cfg ^. _maxHeightToEdge * tan slope

heightToSlope :: SlopeEditorConfig -> Number -> Angle
heightToSlope cfg height = atan2 height (cfg ^. _maxHeightToEdge)

-- | setup drag arrow to edit the house slope
slopeEditor :: forall e. SlopeEditorConfig -> Node e (Event Angle)
slopeEditor conf =
    fixNodeDWith (conf ^. _slope) \slopeDyn -> do
        let actDyn = isActive <$> conf ^. _modeDyn
            pos = conf ^. _position
            min = conf ^. _min
            max = conf ^. _max
            mat = conf ^. _arrowMaterial
            slope = conf ^. _slope

            h = slopeToHeight conf slope

            -- make sure the arrow can only be dragged in a specific range
            validator p = let s = heightToSlope conf (vecZ p + h) in s >= min && s <= max
            -- make sure the arrow can only be draged along Z axis
            transF d = mkVec3 0.0 0.0 (vecZ d)

            cfg = def # _isActive       .~ actDyn
                      # _customGeo      .~ Just arrowGeo
                      # _customMat      .~ mat
                      # _validator      .~ pure validator
                      # _deltaTransform .~ Just transF
                      # _rotation       .~ mkEuler (pi / 2.0) 0.0 0.0

            slopeStrDyn = showSlope <$> slopeDyn
            tWrpCfg = def # _position .~ pure (mkVec3 (-0.8) 0.5 0.0)
            tProp = def # _fontSize  .~ 0.6
                        # _textAlign .~ "center"

        arrow <- node (def # _position .~ pure (moveUp 2.0 pos)
                           # _visible  .~ actDyn) $
                        createDraggableObjectWith cfg $
                            void $ node tWrpCfg $ dynText3D tProp slopeStrDyn
        
        let slopeEvt = heightToSlope conf <<< (+) h <<< vecZ <$> arrow ^. _position

        pure { input: slopeEvt, output: slopeEvt }
