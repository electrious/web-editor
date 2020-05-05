module Editor.WebEditor where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import Control.Plus (empty)
import Data.Array (cons)
import Data.Default (class Default, def)
import Data.Foldable (sequence_)
import Data.Lens (Lens', (%~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple, fst)
import Editor.ArrayBuilder (PanelTextureInfo)
import Editor.Common.Lenses (_disposable)
import Editor.Disposable (class Disposable)
import Editor.EditorMode (EditorMode(..))
import Editor.SceneEvent (Size, size)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import FRP.Event (Event, makeEvent, subscribe)
import Model.Roof.Panel (Panel)
import Model.Roof.RoofPlate (RoofPlate)
import Web.DOM (Element)

newtype EditorConfig = EditorConfig {
    elem       :: Maybe Element,
    defSize    :: Size,
    sizeEvt    :: Event Size,
    defMode    :: EditorMode,
    modeEvt    :: Event EditorMode,
    leadId     :: Int,
    roofPlates :: Array RoofPlate,
    panels     :: Array Panel,
    dataServer :: String,
    textures   :: PanelTextureInfo
}

derive instance newtypeEditorConfig :: Newtype EditorConfig _
instance defaultEditorConfig :: Default EditorConfig where
    def = EditorConfig {
        elem       : Nothing,
        defSize    : size 800 600,
        sizeEvt    : empty,
        defMode    : Showing,
        modeEvt    : empty,
        leadId     : 0,
        roofPlates : [],
        panels     : [],
        dataServer : "",
        textures   : def
    }

_elem :: Lens' EditorConfig (Maybe Element)
_elem = _Newtype <<< prop (SProxy :: SProxy "elem")

_defSize :: Lens' EditorConfig Size
_defSize = _Newtype <<< prop (SProxy :: SProxy "defSize")

_sizeEvt :: Lens' EditorConfig (Event Size)
_sizeEvt = _Newtype <<< prop (SProxy :: SProxy "sizeEvt")

_defMode :: Lens' EditorConfig EditorMode
_defMode = _Newtype <<< prop (SProxy :: SProxy "defMode")

_modeEvt :: Lens' EditorConfig (Event EditorMode)
_modeEvt = _Newtype <<< prop (SProxy :: SProxy "modeEvt")

_roofPlates :: Lens' EditorConfig (Array RoofPlate)
_roofPlates = _Newtype <<< prop (SProxy :: SProxy "roofPlates")

_panels :: Lens' EditorConfig (Array Panel)
_panels = _Newtype <<< prop (SProxy :: SProxy "panels")

_dataServer :: Lens' EditorConfig String
_dataServer = _Newtype <<< prop (SProxy :: SProxy "dataServer")

_textures :: Lens' EditorConfig PanelTextureInfo
_textures = _Newtype <<< prop (SProxy :: SProxy "textures")

-- | Public interface for the main WebEditor
newtype WebEditorState = WebEditorState {
    disposable :: Array (Effect Unit)
}

derive instance newtypeWebEditorState :: Newtype WebEditorState _
instance defaultWebEditorState :: Default WebEditorState where
    def = WebEditorState {
        disposable : []
    }
instance disposableEditorState :: Disposable WebEditorState where
    dispose s = sequence_ $ s ^. _disposable

newtype WebEditor a = WebEditor (ReaderT EditorConfig (StateT WebEditorState Effect) a)

derive newtype instance functorWebEditor     :: Functor WebEditor
derive newtype instance applyWebEditor       :: Apply WebEditor
derive newtype instance applicativeWebEditor :: Applicative WebEditor
derive newtype instance bindWebEditor        :: Bind WebEditor
derive newtype instance monadWebEditor       :: Monad WebEditor
derive newtype instance monadEffectWebEditor :: MonadEffect WebEditor
derive newtype instance monadAskWebEditor    :: MonadAsk EditorConfig WebEditor
derive newtype instance monadReaderWebEditor :: MonadReader EditorConfig WebEditor
derive newtype instance monadStateWebEditor  :: MonadState WebEditorState WebEditor

runWebEditor :: forall a. EditorConfig -> WebEditor a -> Effect (Tuple a WebEditorState)
runWebEditor cfg editor = runWebEditor' cfg def editor

runWebEditor' :: forall a. EditorConfig -> WebEditorState -> WebEditor a -> Effect (Tuple a WebEditorState)
runWebEditor' cfg st (WebEditor editor) = runStateT (runReaderT editor cfg) st

evalWebEditor' :: forall a. EditorConfig -> WebEditorState -> WebEditor a -> Effect a
evalWebEditor' cfg st editor = fst <$> runWebEditor' cfg st editor

performEditorEvent :: forall a. Event (WebEditor a) -> WebEditor (Event a)
performEditorEvent e = do
    cfg <- ask
    st  <- get

    pure $ makeEvent \k -> subscribe e (\v -> evalWebEditor' cfg st v >>= k)


addDisposable :: Effect Unit -> WebEditor Unit
addDisposable d = modify_ (\s -> s # _disposable %~ (cons d))
