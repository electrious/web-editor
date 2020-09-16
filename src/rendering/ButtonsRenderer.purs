module Editor.Rendering.ButtonsRenderer (ButtonOperation(..),
    ButtonsRenderer, _plusTapped, _plusDragged, _rotTapped,
    mkButtonsRenderer) where

import Prelude hiding (add)

import Control.Monad.Reader (ask)
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), find, fromFoldable, partition)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, traverse_)
import Editor.Common.Lenses (_dragged, _id, _tapped)
import Editor.UI.DragInfo (DragInfo)
import Editor.UI.PlusButton (PlusButtonNode, _plusButton, moveBy)
import Editor.UI.RotateButton (RotateButtonNode)
import Effect (Effect)
import FRP.Event (Event, keepLatest)
import FRP.Event.Extra (foldEffect, leftmost, multicast)
import Model.PlusButton (PlusButton)
import Model.RotateButton (RotateButton)
import Rendering.Renderable (RendererConfig, RenderingM, render, runRenderingM)
import Three.Core.Object3D (class IsObject3D, add, remove)
import Three.Math.Vector (Vector3)

-- Operations to update the buttons renderer
data ButtonOperation = RenderPlusButtons (List PlusButton)
                     | RenderRotateButtons (List RotateButton)
                     | HideButtonsExcept PlusButton
                     | MovePlusButton PlusButton Vector3
                     | ResetButtons

derive instance genericButtonOperation :: Generic ButtonOperation _
instance showButtonOperation :: Show ButtonOperation where
    show = genericShow

newtype ButtonsRenderer = ButtonsRenderer {
    plusTapped  :: Event PlusButton,
    plusDragged :: Event (DragInfo PlusButton),
    rotTapped   :: Event RotateButton
}

derive instance newtypeButtonsRenderer :: Newtype ButtonsRenderer _

_plusTapped :: forall t a r. Newtype t { plusTapped :: a | r } => Lens' t a
_plusTapped = _Newtype <<< prop (SProxy :: SProxy "plusTapped")

_plusDragged :: forall t a r. Newtype t { plusDragged :: a | r } => Lens' t a
_plusDragged = _Newtype <<< prop (SProxy :: SProxy "plusDragged")

_rotTapped :: forall t a r. Newtype t { rotTapped :: a | r } => Lens' t a
_rotTapped = _Newtype <<< prop (SProxy :: SProxy "rotTapped")


newtype ButtonRendererState = ButtonRendererState {
    plusBtns :: List PlusButtonNode,
    rotBtns  :: List RotateButtonNode
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

-- | helper function to apply buttons operation and update the internal state
applyOp :: forall a. IsObject3D a => a -> RendererConfig -> ButtonOperation -> ButtonRendererState -> Effect ButtonRendererState
applyOp parent cfg (RenderPlusButtons ps) st = do
    nst   <- delPlusNodes parent st
    nodes <- runRenderingM (traverse render ps) cfg
    traverse_ (flip add parent) nodes
    pure $ nst # _plusBtns .~ nodes
applyOp parent cfg (RenderRotateButtons rs) st = do
    nst   <- delRotNodes parent st
    nodes <- runRenderingM (traverse render rs) cfg
    traverse_ (flip add parent) nodes
    pure $ nst # _rotBtns .~ nodes
applyOp parent cfg (HideButtonsExcept pb) st = do
    let f p = p ^. _plusButton <<< _id /= pb ^. _id
        hiddenBtns = partition f (st ^. _plusBtns)
    traverse_ (flip remove parent) hiddenBtns.yes
    nst <- delRotNodes parent st
    pure $ nst # _plusBtns .~ hiddenBtns.no
               # _rotBtns  .~ Nil
applyOp parent cfg (MovePlusButton pb delta) st = do
    let pn = find ((==) (pb ^. _id) <<< view (_plusButton <<< _id)) $ st ^. _plusBtns
    npn <- traverse (moveBy delta) pn
    pure $ st # _plusBtns .~ fromFoldable npn
applyOp parent cfg ResetButtons st = do
    nst <- delPlusNodes parent st
    _ <- delRotNodes parent nst
    pure $ st # _plusBtns .~ Nil
              # _rotBtns  .~ Nil

-- delete all plus buttons
delPlusNodes :: forall a. IsObject3D a => a -> ButtonRendererState -> Effect ButtonRendererState
delPlusNodes parent st = do
    traverse_ (flip remove parent) (st ^. _plusBtns)
    pure $ st # _plusBtns .~ Nil

-- delete all rotate buttons
delRotNodes :: forall a. IsObject3D a => a -> ButtonRendererState -> Effect ButtonRendererState
delRotNodes parent st = do
    traverse_ (flip remove parent) (st ^. _rotBtns)
    pure $ st # _rotBtns .~ Nil

-- | create buttons renderer that will render all buttons
mkButtonsRenderer :: forall a. IsObject3D a => a -> Event ButtonOperation -> RenderingM ButtonsRenderer
mkButtonsRenderer parent opEvt = do
    cfg <- ask

    let stateEvt     = multicast $ foldEffect (applyOp parent cfg) opEvt def
        plusNodesEvt = multicast $ view _plusBtns <$> stateEvt
        rotNodesEvt  = view _rotBtns <$> stateEvt
    pure $ ButtonsRenderer {
        plusTapped  : multicast $ keepLatest $ leftmost <<< map (view _tapped)  <$> plusNodesEvt,
        plusDragged : multicast $ keepLatest $ leftmost <<< map (view _dragged) <$> plusNodesEvt,
        rotTapped   : multicast $ keepLatest $ leftmost <<< map (view _tapped)  <$> rotNodesEvt
    }
