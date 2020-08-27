module Editor.PanelLayer where

import Prelude hiding (add)

import Control.Plus (empty)
import Data.Default (def)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, traverse_)
import Editor.ArrayBuilder (ArrayBuilder)
import Editor.Common.Lenses (_disposable, _object, _roof)
import Editor.Disposable (class Disposable)
import Editor.PanelAPIInterpreter (PanelAPIInterpreter(..), mkPanelAPIInterpreter)
import Editor.PanelArrayLayout (PanelsLayout(..))
import Editor.PanelNode (PanelOpacity, mkPanelNode)
import Editor.PanelOperation (ArrayOperation)
import Editor.Rendering.ButtonsRenderer (mkButtonsRenderer)
import Editor.Rendering.PanelRendering (PanelRenderer, PanelRendererConfig(..), _opacity, createPanelRenderer)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic(..), mergeDynArray, subscribeDyn, withLast)
import FRP.Event (Event, fix)
import Model.Hardware.PanelModel (PanelModel(..))
import Model.Roof.ArrayConfig (ArrayConfig(..))
import Model.Roof.Panel (Orientation, Panel)
import Model.Roof.RoofPlate (RoofPlate(..))
import Three.Core.Object3D (class IsObject3D, Object3D, add, mkObject3D, remove, setName, toObject3D)

newtype PanelLayerConfig = PanelLayerConfig {
    roof            :: RoofPlate,
    roofActive      :: Dynamic Boolean,
    mainOrientation :: Dynamic Orientation,
    arrayConfig     :: Dynamic ArrayConfig,
    panelType       :: Dynamic PanelModel,
    initPanels      :: Event (List Panel),
    opacity         :: Dynamic PanelOpacity
}

derive instance newtypePanelLayerConfig :: Newtype PanelLayerConfig _

_roofActive :: forall t a r. Newtype t { roofActive :: a | r } => Lens' t a
_roofActive = _Newtype <<< prop (SProxy :: SProxy "roofActive")

_mainOrientation :: forall t a r. Newtype t { mainOrientation :: a | r } => Lens' t a
_mainOrientation = _Newtype <<< prop (SProxy :: SProxy "mainOrientation")

_initPanels :: forall t a r. Newtype t { initPanels :: a | r } => Lens' t a
_initPanels = _Newtype <<< prop (SProxy :: SProxy "initPanels")

newtype PanelLayer = PanelLayer {
    object              :: Object3D,
    disposable          :: Effect Unit,

    roof                :: RoofPlate, 
    arrayConfig         :: Dynamic ArrayConfig,

    layout              :: Dynamic PanelsLayout,

    arrayChanged        :: Event Unit,
    serverUpdated       :: Event Unit,
    arrayDragging       :: Dynamic Boolean,
    inactiveArrayTapped :: Event Unit
}

derive instance newtypePanelLayer :: Newtype PanelLayer _
instance isObject3DPanelLayer :: IsObject3D PanelLayer where
    toObject3D = view _object
instance disposablePanelLayer :: Disposable PanelLayer where
    dispose = view _disposable

_layout :: forall t a r. Newtype t { layout :: a | r } => Lens' t a
_layout = _Newtype <<< prop (SProxy :: SProxy "layout")

_arrayChanged :: forall t a r. Newtype t { arrayChanged :: a | r } => Lens' t a
_arrayChanged = _Newtype <<< prop (SProxy :: SProxy "arrayChanged")

_serverUpdated :: forall t a r. Newtype t {serverUpdated :: a | r } => Lens' t a
_serverUpdated = _Newtype <<< prop (SProxy :: SProxy "serverUpdated")

_arrayDragging :: forall t a r. Newtype t { arrayDragging :: a | r } => Lens' t a
_arrayDragging = _Newtype <<< prop (SProxy :: SProxy "arrayDragging")

_inactiveArrayTapped :: forall t a r. Newtype t { inactiveArrayTapped :: a | r } => Lens' t a
_inactiveArrayTapped = _Newtype <<< prop (SProxy :: SProxy "inactiveArrayTapped")

createPanelLayer :: PanelLayerConfig -> ArrayBuilder PanelLayer
createPanelLayer cfg = do
    layer <- liftEffect mkObject3D
    liftEffect $ setName "panel-layer" layer

    let arrOpEvt = empty

    panelRenderer <- setupPanelRenderer layer arrOpEvt (cfg ^. _opacity)
    btnsRenderer  <- mkButtonsRenderer layer btnOpEvt
    let apiInterpreter = mkPanelAPIInterpreter def


    pure $ PanelLayer {
        object     : layer,
        disposable : pure unit,

        roof       : cfg ^. _roof
    }


setupPanelRenderer :: Object3D -> Event ArrayOperation -> Dynamic PanelOpacity -> ArrayBuilder PanelRenderer
setupPanelRenderer parent opEvt opacity = createPanelRenderer $ PanelRendererConfig {
        parent     : parent,
        operations : opEvt,
        opacity    : opacity
    }