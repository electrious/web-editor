module Rendering.Node where

import Prelude hiding (add)

import Control.Alternative (empty)
import Control.Monad.RWS (tell)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.Writer (class MonadTell, class MonadWriter, WriterT, runWriterT)
import Custom.Mesh (DraggableMesh, TapDragMesh, TappableMesh, mkDraggableMesh, mkTapDragMesh, mkTappableMesh)
import Data.Default (class Default)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Editor.Common.Lenses (_name, _position, _rotation, _scale)
import Editor.Disposable (Disposee(..), dispose)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Dynamic (Dynamic, performDynamic, subscribeDyn, withLast)
import FRP.Event (Event, subscribe)
import Three.Core.Geometry (class IsGeometry)
import Three.Core.Material (class IsMaterial)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (class IsObject3D, Object3D, add, mkObject3D, remove, setCastShadow, setName, setPosition, setReceiveShadow, setRenderOrder, setRotation, setScale, setVisible, toObject3D)
import Three.Math.Euler (Euler)
import Three.Math.Vector (Vector3)

newtype Node a = Node (ReaderT Object3D (WriterT Disposee Effect) a)

derive instance newtypeNode :: Newtype (Node a) _

derive newtype instance functorNode     :: Functor Node
derive newtype instance applyNode       :: Apply Node
derive newtype instance applicativeNode :: Applicative Node
derive newtype instance bindNode        :: Bind Node
derive newtype instance monadNode       :: Monad Node
derive newtype instance monadEffectNode :: MonadEffect Node
derive newtype instance monadAskNode    :: MonadAsk Object3D Node
derive newtype instance monadReaderNode :: MonadReader Object3D Node
derive newtype instance monadTellNode   :: MonadTell Disposee Node
derive newtype instance monadWriterNode :: MonadWriter Disposee Node

runNode :: forall a. Node a -> Object3D -> Effect (Tuple a Disposee)
runNode (Node r) = runWriterT <<< runReaderT r

-- Define Node properties
newtype Props = Props {
    name          :: String,
    castShadow    :: Boolean,
    receiveShadow :: Boolean,
    visible       :: Boolean,
    renderOrder   :: Int,
    position      :: Event Vector3,
    rotation      :: Event Euler,
    scale         :: Event Vector3
    }

derive instance newtypeProps :: Newtype Props _

instance defaultProps :: Default Props where
    def = Props {
        name          : "node",
        castShadow    : true,
        receiveShadow : true,
        visible       : true,
        renderOrder   : 0,
        position      : empty,
        rotation      : empty,
        scale         : empty
        }

_castShadow :: forall t a r. Newtype t { castShadow :: a | r } => Lens' t a
_castShadow = _Newtype <<< prop (SProxy :: SProxy "castShadow")

_receiveShadow :: forall t a r. Newtype t { receiveShadow :: a | r } => Lens' t a
_receiveShadow = _Newtype <<< prop (SProxy :: SProxy "receiveShadow")

_visible :: forall t a r. Newtype t { visible :: a | r } => Lens' t a
_visible = _Newtype <<< prop (SProxy :: SProxy "visible")

_renderOrder :: forall t a r. Newtype t { renderOrder :: a | r } => Lens' t a
_renderOrder = _Newtype <<< prop (SProxy :: SProxy "renderOrder")

setupProps :: forall o. IsObject3D o => Props -> o -> Effect Disposee
setupProps prop o = do
    setName          (prop ^. _name) o
    setCastShadow    (prop ^. _castShadow) o
    setReceiveShadow (prop ^. _receiveShadow) o
    setVisible       (prop ^. _visible) o
    setRenderOrder   (prop ^. _renderOrder) o

    d1 <- subscribe (prop ^. _position) (flip setPosition o)
    d2 <- subscribe (prop ^. _rotation) (flip setRotation o)
    d3 <- subscribe (prop ^. _scale) (flip setScale o)
    
    pure $ Disposee $ d1 *> d2 *> d3

-- internal helper function to create node functions with specified node maker function
mkNode :: forall a m. IsObject3D m => Props -> Node a -> Effect m -> Node (Tuple a m)
mkNode prop child func = do
    m <- liftEffect func
    parent <- ask

    liftEffect $ add m parent

    d <- liftEffect $ setupProps prop m

    -- remove the object from parent if node is disposed
    tell $ d <> Disposee (remove m parent)
    
    -- run child action with the new oobject as parent
    r <- local (const (toObject3D m)) child
    pure $ Tuple r m

node :: forall a. Props -> Node a -> Node a
node prop child = map fst $ mkNode prop child mkObject3D

mesh :: forall geo mat a. IsGeometry geo => IsMaterial mat => Props -> geo -> mat -> Node a -> Node (Tuple a Mesh)
mesh prop geo mat child = mkNode prop child $ mkMesh geo mat

mesh' :: forall geo mat a. IsGeometry geo => IsMaterial mat => Props -> geo -> mat -> Node a -> Node a
mesh' prop geo mat = map fst <<< mesh prop geo mat

tapMesh :: forall geo mat a. IsGeometry geo => IsMaterial mat => Props -> geo -> mat -> Node a -> Node (Tuple a TappableMesh)
tapMesh prop geo mat child = mkNode prop child $ mkTappableMesh geo mat

dragMesh :: forall geo mat a. IsGeometry geo => IsMaterial mat => Props -> geo -> mat -> Node a -> Node (Tuple a DraggableMesh)
dragMesh prop geo mat child = mkNode prop child $ mkDraggableMesh geo mat

tapDragMesh :: forall geo mat a. IsGeometry geo => IsMaterial mat => Props -> geo -> mat -> Node a -> Node (Tuple a TapDragMesh)
tapDragMesh prop geo mat child = mkNode prop child $ mkTapDragMesh geo mat


-- create node dynamically
dynamic :: forall a. Dynamic (Node a) -> Node (Dynamic a)
dynamic dyn = do
    parent <- ask

    let rDyn       = performDynamic $ flip runNode parent <$> dyn
        resDyn     = fst <$> rDyn

        getLast o  = o.last

        f Nothing  = pure unit
        f (Just d) = dispose d
        
    d <- liftEffect $ subscribeDyn (withLast $ snd <$> rDyn) (getLast >>> f)
    tell $ Disposee d
    
    pure resDyn

dynamic_ :: forall a. Dynamic (Node a) -> Node Unit
dynamic_ = void <<< dynamic

withDynamic :: forall a b. Dynamic a -> (a -> Node b) -> Node (Dynamic b)
withDynamic dyn f = dynamic $ f <$> dyn

withDynamic_ :: forall a. Dynamic a -> (a -> Node Unit) -> Node Unit
withDynamic_ dyn f = dynamic_ $ f <$> dyn
