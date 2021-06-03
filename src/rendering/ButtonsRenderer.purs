module Editor.Rendering.ButtonsRenderer (ButtonOperation(..), ButtonsEvents,
                                         _plusTapped, _plusDragged, _rotTapped,
                                         mkButtonsRenderer) where

import Prelude hiding (add)

import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), singleton)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_dragged, _tapped)
import Editor.UI.DragInfo (DragInfo)
import Editor.UI.PlusButton (PlusButtonNode)
import Editor.UI.RotateButton (RotateButtonNode)
import FRP.Dynamic (Dynamic, sampleDyn)
import FRP.Event (Event)
import FRP.Event.Extra (multicast)
import Model.PlusButton (PlusButton)
import Model.RotateButton (RotateButton)
import Rendering.DynamicNode (renderDynamic)
import Rendering.Node (Node, fixNodeDWith)
import Three.Core.Material (MeshBasicMaterial)
import Util (latestAnyEvt)

-- Operations to update the buttons renderer
data ButtonOperation = RenderPlusButtons (List PlusButton)
                     | RenderRotateButtons (List RotateButton)
                     | HideButtonsExcept PlusButton
                     | ResetButtons

derive instance genericButtonOperation :: Generic ButtonOperation _
instance showButtonOperation :: Show ButtonOperation where
    show = genericShow

newtype ButtonsEvents = ButtonsEvents {
    plusTapped  :: Event PlusButton,
    plusDragged :: Event (DragInfo PlusButton),
    rotTapped   :: Event RotateButton
}

derive instance newtypeButtonsEvents :: Newtype ButtonsEvents _

_plusTapped :: forall t a r. Newtype t { plusTapped :: a | r } => Lens' t a
_plusTapped = _Newtype <<< prop (SProxy :: SProxy "plusTapped")

_plusDragged :: forall t a r. Newtype t { plusDragged :: a | r } => Lens' t a
_plusDragged = _Newtype <<< prop (SProxy :: SProxy "plusDragged")

_rotTapped :: forall t a r. Newtype t { rotTapped :: a | r } => Lens' t a
_rotTapped = _Newtype <<< prop (SProxy :: SProxy "rotTapped")


newtype ButtonRendererState = ButtonRendererState {
    plusBtns :: List PlusButton,
    rotBtns  :: List RotateButton
}

derive instance newtypeButtonRendererState :: Newtype ButtonRendererState _
instance defaultButtonRendererState :: Default ButtonRendererState where
    def = ButtonRendererState {
        plusBtns : Nil,
        rotBtns  : Nil
    }

_plusBtns :: forall t a r. Newtype t { plusBtns :: a | r } => Lens' t a
_plusBtns = _Newtype <<< prop (SProxy :: SProxy "plusBtns")

_rotBtns :: forall t a r. Newtype t { rotBtns :: a | r } => Lens' t a
_rotBtns = _Newtype <<< prop (SProxy :: SProxy "rotBtns")

applyOp :: ButtonOperation -> ButtonRendererState -> ButtonRendererState
applyOp (RenderPlusButtons bs)   st = st # _plusBtns .~ bs
applyOp (RenderRotateButtons bs) st = st # _rotBtns  .~ bs
applyOp (HideButtonsExcept b)    st = st # _plusBtns .~ singleton b
                                         # _rotBtns  .~ Nil
applyOp ResetButtons             st = def


-- | create buttons renderer that will render all buttons
mkButtonsRenderer :: Event ButtonOperation -> Node MeshBasicMaterial ButtonsEvents
mkButtonsRenderer opEvt = fixNodeDWith def \stDyn -> do
    -- update internal states with operation events
    let newStEvt = sampleDyn stDyn $ applyOp <$> opEvt

        pbsDyn = view _plusBtns <$> stDyn
        rbsDyn = view _rotBtns  <$> stDyn

    -- render buttons
    pbns :: Dynamic (List PlusButtonNode)   <- renderDynamic pbsDyn
    rbns :: Dynamic (List RotateButtonNode) <- renderDynamic rbsDyn
    
    let res = ButtonsEvents {
            plusTapped  : multicast $ latestAnyEvt $ map (view _tapped)  <$> pbns,
            plusDragged : multicast $ latestAnyEvt $ map (view _dragged) <$> pbns,
            rotTapped   : multicast $ latestAnyEvt $ map (view _tapped)  <$> rbns
            }
    pure { input : newStEvt, output : res }
