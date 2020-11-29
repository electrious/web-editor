module Rendering.Node where

import Prelude hiding (add)

import Control.Monad.RWS (tell)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.Writer (class MonadTell, class MonadWriter, WriterT, runWriterT)
import Custom.Mesh (DraggableMesh, TappableMesh, mkDraggableMesh, mkTappableMesh)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst)
import Editor.Disposable (Disposee(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Three.Core.Geometry (class IsGeometry)
import Three.Core.Material (class IsMaterial)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (class IsObject3D, Object3D, add, mkObject3D, remove, setName, toObject3D)

newtype Node a = Node (ReaderT Object3D (WriterT Disposee Effect) a)

derive instance newtypeNode :: Newtype (Node a) _

derive newtype instance functorNode :: Functor Node
derive newtype instance applyNode :: Apply Node
derive newtype instance applicativeNode :: Applicative Node
derive newtype instance bindNode :: Bind Node
derive newtype instance monadNode :: Monad Node
derive newtype instance monadEffectNode :: MonadEffect Node
derive newtype instance monadAskNode :: MonadAsk Object3D Node
derive newtype instance monadReaderNode :: MonadReader Object3D Node
derive newtype instance monadTellNode :: MonadTell Disposee Node
derive newtype instance monadWriterNode :: MonadWriter Disposee Node

runNode :: forall a. Node a -> Object3D -> Effect (Tuple a Disposee)
runNode (Node r) = runWriterT <<< runReaderT r

-- internal helper function to create node functions with specified node maker function
mkNode :: forall a m. IsObject3D m => Node a -> Effect m -> Node (Tuple a m)
mkNode child func = do
    m <- liftEffect func
    parent <- ask

    liftEffect $ add m parent

    -- remove the object from parent if node is disposed
    tell $ Disposee $ remove m parent
    
    -- run child action with the new oobject as parent
    r <- local (const (toObject3D m)) child
    pure $ Tuple r m
    

node :: forall a. String -> Node a -> Node a
node name child = map fst $ mkNode child do
    w <- mkObject3D
    setName name w
    pure w

mesh :: forall geo mat a. IsGeometry geo => IsMaterial mat => String -> geo -> mat -> Node a -> Node (Tuple a Mesh)
mesh name geo mat child = mkNode child do
    m <-mkMesh geo mat
    setName name m
    pure m


mesh' :: forall geo mat a. IsGeometry geo => IsMaterial mat => String -> geo -> mat -> Node a -> Node a
mesh' name geo mat = map fst <<< mesh name geo mat

tapMesh :: forall geo mat a. IsGeometry geo => IsMaterial mat => String -> geo -> mat -> Node a -> Node (Tuple a TappableMesh)
tapMesh name geo mat child = mkNode child do
    m <- mkTappableMesh geo mat
    setName name m
    pure m

dragMesh :: forall geo mat a. IsGeometry geo => IsMaterial mat => String -> geo -> mat -> Node a -> Node (Tuple a DraggableMesh)
dragMesh name geo mat child = mkNode child do
    m <- mkDraggableMesh geo mat
    setName name m
    pure m
