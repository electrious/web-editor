module UI.ArrayEditorUI where

import Prelude hiding (div)

import Data.Default (class Default, def)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_alignment)
import Editor.HouseEditor (ArrayEditParam, _heatmap)
import Editor.PanelNode (PanelOpacity(..))
import Editor.Rendering.PanelRendering (_opacity)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic)
import Model.ActiveMode (ActiveMode(..))
import Model.Roof.Panel (Alignment(..))
import Specular.Dom.Element (classes, el, text)
import Specular.Dom.Widget (Widget, emptyWidget)
import UI.Bridge (fromUIEvent, toUIDyn)
import UI.Switcher (switcher)

newtype ArrayEditorUIOpt = ArrayEditorUIOpt {
    alignment        :: Dynamic Alignment,
    alignmentEnabled :: Dynamic ActiveMode,
    opacity          :: Dynamic PanelOpacity,
    heatmap          :: Dynamic Boolean
    }

derive instance newtypeArrayEditorUIOpt :: Newtype ArrayEditorUIOpt _
instance defaultArrayEditorUIOpt :: Default ArrayEditorUIOpt where
    def = ArrayEditorUIOpt {
        alignment        : pure Grid,
        alignmentEnabled : pure Inactive,
        opacity          : pure Opaque,
        heatmap          : pure false
        }

_alignmentEnabled :: forall t a r. Newtype t { alignmentEnabled :: a | r } => Lens' t a
_alignmentEnabled = _Newtype <<< prop (SProxy :: SProxy "alignmentEnabled")


arrayEditorPane :: ArrayEditorUIOpt -> Widget ArrayEditParam
arrayEditorPane opt = do
    alignDyn       <- liftEffect $ toUIDyn $ opt ^. _alignment
    alignEnableDyn <- liftEffect $ toUIDyn $ opt ^. _alignmentEnabled
    opacityDyn     <- liftEffect $ toUIDyn $ opt ^. _opacity
    heatmapDyn     <- liftEffect $ toUIDyn $ opt ^. _heatmap

    el "table" [classes ["uk-table", "uk-table-small"]] $
        el "tbody" [] do
            alignEvtUI <- el "tr" [] do
                el "td" [] $ text "Alignment:"
                el "td" [] $ switcher [Grid, Brick] alignDyn alignEnableDyn
            opacityEvtUI <- el "tr" [] do
                el "td" [] $ text "Visibility:"
                el "td" [] $ switcher [Opaque, Transparent] opacityDyn (pure Active)
            hmEvtUI <- el "tr" [] do
                el "td" [] $ text "Heatmap:"
                el "td" [] $ switcher [true, false] heatmapDyn (pure Active)
            el "tr" [] do
                el "td" [] $ el "b" [] $ text "Important:"
                el "td" [] do
                    text "Array changes will be saved automatically."
                    el "br" [] emptyWidget
                    text "No need to click \"Save\" or refresh the page."

            newAlignEvt <- fromUIEvent alignEvtUI
            opacityEvt  <- fromUIEvent opacityEvtUI
            hmEvt       <- fromUIEvent hmEvtUI
            
            pure $ def # _alignment .~ newAlignEvt
                       # _opacity   .~ opacityEvt
                       # _heatmap   .~ hmEvt
