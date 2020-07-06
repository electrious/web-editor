module Editor.EditorM where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, runReaderT)
import Control.Plus (empty)
import Data.Default (class Default)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.EditorMode (EditorMode(..))
import Editor.SceneEvent (Size, size)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import FRP.Dynamic (Dynamic, step)
import Three.Math.Vector (Vector3)
import Web.DOM (Element)

newtype EditorConfig = EditorConfig {
    elem            :: Maybe Element,
    sizeDyn         :: Dynamic Size,
    modeDyn         :: Dynamic EditorMode,
    flyCameraTarget :: Dynamic (Maybe Vector3)
}

derive instance newtypeEditorConfig :: Newtype EditorConfig _
instance defaultEditorConfig :: Default EditorConfig where
    def = EditorConfig {
        elem            : Nothing,
        sizeDyn         : step (size 800 600) empty,
        modeDyn         : step Showing empty,
        flyCameraTarget : step Nothing empty
    }

_elem :: Lens' EditorConfig (Maybe Element)
_elem = _Newtype <<< prop (SProxy :: SProxy "elem")

_sizeDyn :: Lens' EditorConfig (Dynamic Size)
_sizeDyn = _Newtype <<< prop (SProxy :: SProxy "sizeDyn")

_flyCameraTarget :: Lens' EditorConfig (Dynamic (Maybe Vector3))
_flyCameraTarget = _Newtype <<< prop (SProxy :: SProxy "flyCameraTarget")

newtype EditorM a = EditorM (ReaderT EditorConfig Effect a)

derive newtype instance functorEditorM     :: Functor EditorM
derive newtype instance applyEditorM       :: Apply EditorM
derive newtype instance applicativeEditorM :: Applicative EditorM
derive newtype instance bindEditorM        :: Bind EditorM
derive newtype instance monadEditorM       :: Monad EditorM
derive newtype instance monadEffectEditorM :: MonadEffect EditorM
derive newtype instance monadAskEditorM    :: MonadAsk EditorConfig EditorM
derive newtype instance monadReaderEditorM :: MonadReader EditorConfig EditorM

runEditorM :: forall a. EditorM a -> EditorConfig -> Effect a
runEditorM (EditorM editor) = runReaderT editor
