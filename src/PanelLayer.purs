module Editor.PanelLayer where

import Prelude hiding (add)

import Data.Lens (view)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse, traverse_)
import Editor.ArrayBuilder (ArrayBuilder)
import Editor.Common.Lenses (_object)
import Editor.PanelArrayLayout (PanelsLayout(..))
import Editor.PanelNode (mkPanelNode)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic(..), mergeDynArray, subscribeDyn, withLast)
import Model.Hardware.PanelModel (PanelModel(..))
import Model.Roof.ArrayConfig (ArrayConfig(..))
import Model.Roof.Panel (Orientation, Panel)
import Model.Roof.RoofPlate (RoofPlate(..))
import Three.Core.Object3D (class IsObject3D, Object3D, add, mkObject3D, remove, setName, toObject3D)

newtype PanelLayerConfig = PanelLayerConfig {
    roofActive      :: Dynamic Boolean,
    mainOrientatoin :: Dynamic Orientation,
    arrayConfig     :: Dynamic ArrayConfig,
    panelType       :: Dynamic PanelModel
}

derive instance newtypePanelLayerConfig :: Newtype PanelLayerConfig _

newtype PanelLayer = PanelLayer {
    object      :: Object3D,
    disposable  :: Effect Unit,

    roofplate   :: RoofPlate, 
    arrayConfig :: Dynamic ArrayConfig,

    layout      :: Dynamic PanelsLayout
}

derive instance newtypePanelLayer :: Newtype PanelLayer _
instance isObject3DPanelLayer :: IsObject3D PanelLayer where
    toObject3D = view _object

createPanelLayer :: Array Panel -> ArrayBuilder PanelLayer
createPanelLayer ps = do
    layer <- liftEffect mkObject3D
    liftEffect $ setName "panel-layer" layer


    pure $ PanelLayer {
        object     : layer,
        disposable : d
    }
