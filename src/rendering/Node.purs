module Rendering.Node where

import Prelude hiding (add)

import Control.Monad.RWS (tell)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.Writer (class MonadTell, class MonadWriter, WriterT, runWriterT)
import Custom.Mesh (DraggableMesh, TapDragMesh, TapMouseMesh, TappableMesh, mkDraggableMesh, mkTapDragMesh, mkTapMouseMesh, mkTappableMesh)
import Data.Default (class Default, def)
import Data.Lens (Lens', set, view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_color, _name, _parent, _position, _rotation, _scale)
import Editor.Disposable (Disposee(..), dispose)
import Editor.SceneEvent (setRaycastable)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Dynamic (Dynamic, step, subscribeDyn)
import FRP.Event (Event, create, subscribe)
import Three.Core.Geometry (class IsGeometry, mkLineGeometry)
import Three.Core.Material (class IsLineMaterial, class IsMaterial)
import Three.Core.Mesh (Line, Mesh, computeLineDistances, mkLine, mkMesh)
import Three.Core.Object3D (class IsObject3D, Object3D, add, lookAt, mkObject3D, remove, setCastShadow, setExportable, setName, setPosition, setReceiveShadow, setRenderOrder, setRotation, setScale, setVisible, toObject3D)
import Three.Math.Euler (Euler)
import Three.Math.Vector (Vector3, mkVec3)
import Troika.Text (Text, mkText, setColor, setFontSize, setText, setTextAlign, sync)
import Type.Proxy (Proxy(..))

newtype NodeEnv e = NodeEnv {
    parent :: Object3D,
    env    :: e
}

derive instance newtypeNodeEnv :: Newtype (NodeEnv e) _
instance functorNodeEnv :: Functor NodeEnv where
    map f e = NodeEnv {
                parent : e ^. _parent,
                env    : f (e ^. _env)
              }

_env :: forall t a r. Newtype t { env :: a | r } => Lens' t a
_env = _Newtype <<< prop (Proxy :: Proxy "env")

mkNodeEnv :: forall o e. IsObject3D o => o -> e -> NodeEnv e
mkNodeEnv obj e = NodeEnv { parent : toObject3D obj, env : e }

newtype Node e a = Node (ReaderT (NodeEnv e) (WriterT Disposee Effect) a)

derive instance newtypeNode :: Newtype (Node e a) _

derive newtype instance functorNode     :: Functor (Node e)
derive newtype instance applyNode       :: Apply (Node e)
derive newtype instance applicativeNode :: Applicative (Node e)
derive newtype instance bindNode        :: Bind (Node e)
derive newtype instance monadNode       :: Monad (Node e)
derive newtype instance monadEffectNode :: MonadEffect (Node e)
derive newtype instance monadAskNode    :: MonadAsk (NodeEnv e) (Node e)
derive newtype instance monadReaderNode :: MonadReader (NodeEnv e) (Node e)
derive newtype instance monadTellNode   :: MonadTell Disposee (Node e)
derive newtype instance monadWriterNode :: MonadWriter Disposee (Node e)

-- | run a Node action with NodeEnv
runNode :: forall e a. Node e a -> NodeEnv e -> Effect (Tuple a Disposee)
runNode (Node r) = runWriterT <<< runReaderT r

-- | run a child node action in a parent context that has different env value.
localEnv :: forall ep ec a. (ep -> ec) -> Node ec a -> Node ep a
localEnv f c = do
    e <- ask
    Tuple r d <- liftEffect $ runNode c (f <$> e)
    tell d
    pure r

-- | get the env value in Node monad
getEnv :: forall e. Node e e
getEnv = view _env <$> ask

-- | get the parent object in Node monad
getParent :: forall e. Node e Object3D
getParent = view _parent <$> ask

-- Define Node properties
newtype Props = Props {
    name          :: String,
    castShadow    :: Boolean,
    receiveShadow :: Boolean,
    renderOrder   :: Int,
    position      :: Dynamic Vector3,
    rotation      :: Dynamic Euler,
    scale         :: Dynamic Vector3,
    target        :: Dynamic (Maybe Vector3),
    visible       :: Dynamic Boolean,
    raycastable   :: Dynamic Boolean,
    exportable    :: Boolean
    }

derive instance newtypeProps :: Newtype Props _

instance defaultProps :: Default Props where
    def = Props {
        name          : "node",
        castShadow    : true,
        receiveShadow : true,
        renderOrder   : 0,
        position      : pure def,
        rotation      : pure def,
        scale         : pure (mkVec3 1.0 1.0 1.0),
        target        : pure Nothing,
        visible       : pure true,
        raycastable   : pure true,
        exportable    : false
        }

_castShadow :: forall t a r. Newtype t { castShadow :: a | r } => Lens' t a
_castShadow = _Newtype <<< prop (Proxy :: Proxy "castShadow")

_receiveShadow :: forall t a r. Newtype t { receiveShadow :: a | r } => Lens' t a
_receiveShadow = _Newtype <<< prop (Proxy :: Proxy "receiveShadow")

_target :: forall t a r. Newtype t { target :: a | r } => Lens' t a
_target = _Newtype <<< prop (Proxy :: Proxy "target")

_visible :: forall t a r. Newtype t { visible :: a | r } => Lens' t a
_visible = _Newtype <<< prop (Proxy :: Proxy "visible")

_raycastable :: forall t a r. Newtype t { raycastable :: a | r } => Lens' t a
_raycastable = _Newtype <<< prop (Proxy :: Proxy "raycastable")

_renderOrder :: forall t a r. Newtype t { renderOrder :: a | r } => Lens' t a
_renderOrder = _Newtype <<< prop (Proxy :: Proxy "renderOrder")

_exportable :: forall t a r. Newtype t { exportable :: a | r } => Lens' t a
_exportable = _Newtype <<< prop (Proxy :: Proxy "exportable")

setupProps :: forall o. IsObject3D o => Props -> o -> Effect (Effect Unit)
setupProps prop o = do
    setName          (prop ^. _name) o
    setCastShadow    (prop ^. _castShadow) o
    setReceiveShadow (prop ^. _receiveShadow) o
    setRenderOrder   (prop ^. _renderOrder) o

    when (prop ^. _exportable) $ setExportable o
    
    d1 <- subscribeDyn (prop ^. _position) (flip setPosition o)
    d2 <- subscribeDyn (prop ^. _rotation) (flip setRotation o)
    d3 <- subscribeDyn (prop ^. _scale) (flip setScale o)
    d4 <- subscribeDyn (prop ^. _target) (traverse (flip lookAt o))
    d5 <- subscribeDyn (prop ^. _visible) (flip setVisible o)
    d6 <- subscribeDyn (prop ^. _raycastable) (setRaycastable o)
    
    pure $ d1 *> d2 *> d3 *> d4 *> d5 *> d6

-- internal helper function to create node functions with node maker function
mkNode :: forall e m. IsObject3D m => Props -> Effect m -> Node e m
mkNode prop func = do
    m <- liftEffect func
    parent <- getParent

    liftEffect $ add m parent

    d <- liftEffect $ setupProps prop m

    -- remove the object from parent if node is disposed
    tell $ Disposee $ d *> remove m parent
    pure m


-- | run a child node action with the specified Object3D as parent
runChild :: forall e a m. IsObject3D m => Node e a -> m -> Node e a
runChild child m = local (set _parent (toObject3D m)) child

-- empty node
leaf :: forall e. Node e Unit
leaf = pure unit

node :: forall e a. Props -> Node e a -> Node e a
node prop child = mkNode prop mkObject3D >>= runChild child

mesh :: forall geo mat e. IsGeometry geo => IsMaterial mat => Props -> geo -> mat -> Node e Mesh
mesh prop geo mat = mkNode prop $ mkMesh geo mat

line :: forall e mat. IsLineMaterial mat => Props -> Array Vector3 -> mat -> Node e Line
line prop vs mat = mkNode (prop # _raycastable .~ pure false) $ mkLineGeometry vs >>= flip mkLine mat

dashLine :: forall e mat. IsLineMaterial mat => Props -> Array Vector3 -> mat -> Node e Line
dashLine prop vs mat = mkNode (prop # _raycastable .~ pure false) do
    geo <- mkLineGeometry vs
    l <- mkLine geo mat
    computeLineDistances l
    pure l

tapMesh :: forall geo mat e. IsGeometry geo => IsMaterial mat => Props -> geo -> mat -> Node e TappableMesh
tapMesh prop geo mat = mkNode prop $ mkTappableMesh geo mat

dragMesh :: forall geo mat e. IsGeometry geo => IsMaterial mat => Props -> geo -> mat -> Node e DraggableMesh
dragMesh prop geo mat = mkNode prop $ mkDraggableMesh geo mat

tapDragMesh :: forall geo mat e. IsGeometry geo => IsMaterial mat => Props -> geo -> mat -> Node e TapDragMesh
tapDragMesh prop geo mat = mkNode prop $ mkTapDragMesh geo mat

tapMouseMesh :: forall geo mat e. IsGeometry geo => IsMaterial mat => Props -> geo -> mat -> Node e TapMouseMesh
tapMouseMesh prop geo mat = mkNode prop $ mkTapMouseMesh geo mat


newtype TextProps = TextProps {
    fontSize  :: Number,
    color     :: Int,
    textAlign :: String
}

derive instance newtypeTextProps :: Newtype TextProps _
instance defaultTextProps :: Default TextProps where
    def = TextProps {
        fontSize  : 1.0,
        color     : 0xffffff,
        textAlign : "left"
    }

_fontSize :: forall t a r. Newtype t { fontSize :: a | r } => Lens' t a
_fontSize = _Newtype <<< prop (Proxy :: Proxy "fontSize")

_textAlign :: forall t a r. Newtype t { textAlign :: a | r } => Lens' t a
_textAlign = _Newtype <<< prop (Proxy :: Proxy "textAlign")

text3D :: forall e. TextProps -> String -> Node e Text
text3D prop s = do
    t <- liftEffect mkText
    parent <- getParent

    liftEffect do
        add t parent

        setFontSize (prop ^. _fontSize) t
        setColor (prop ^. _color) t
        setTextAlign (prop ^. _textAlign) t

        setText s t
        sync t

    tell $ Disposee $ remove t parent *> dispose t

    pure t

dynText3D :: forall e. TextProps -> Dynamic String -> Node e Text
dynText3D prop dyn = do
    t <- liftEffect mkText
    parent <- getParent

    liftEffect do
        add t parent

        setFontSize (prop ^. _fontSize) t
        setColor (prop ^. _color) t
        setTextAlign (prop ^. _textAlign) t

    d <- liftEffect $ subscribeDyn dyn \s -> do
        setText s t
        sync t


    tell $ Disposee $ d *> remove t parent *> dispose t

    pure t

-- | compute fixed point in Node context
fixNodeE :: forall e i o. (Event i -> Node e { input :: Event i, output :: o }) -> Node e o
fixNodeE f = do
    { event: inEvt, push: pushInput } <- liftEffect create
    { input: newInEvt, output: out } <- f inEvt
    d <- liftEffect $ subscribe newInEvt pushInput
    tell $ Disposee d
    pure out

-- | compute fixed point with default value in Node context
fixNodeEWith :: forall e i o. i -> (Event i -> Node e { input :: Event i, output :: o }) -> Node e o
fixNodeEWith v f = do
    { event: inEvt, push: pushInput } <- liftEffect create
    { input: newInEvt, output: out } <- f inEvt
    d <- liftEffect $ subscribe newInEvt pushInput
    tell $ Disposee d

    liftEffect $ pushInput v

    pure out

fixNodeDWith :: forall e i o. i -> (Dynamic i -> Node e { input :: Event i, output :: o }) -> Node e o
fixNodeDWith v f = do
    { event: inEvt, push: pushInput } <- liftEffect create
    let inDyn = step v inEvt
    { input: newInEvt, output: out } <- f inDyn
    d <- liftEffect $ subscribe newInEvt pushInput
    tell $ Disposee d
    
    pure out
