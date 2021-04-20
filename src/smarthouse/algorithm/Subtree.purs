module Smarthouse.Algorithm.Subtree where

import Prelude

import Data.Array (elem, foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), (!!))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Math.LineSeg (LineSeg, mkLineSeg)
import Three.Math.Vector (Vector3)

newtype Subtree = Subtree {
    source :: Vector3,
    height :: Number,
    sinks  :: List Vector3,
    edges  :: List (LineSeg Vector3)
    }

derive instance newtypeSubtree :: Newtype Subtree _
derive instance genericSubtree :: Generic Subtree _
instance showSubtree :: Show Subtree where
    show = genericShow

_source :: forall t a r. Newtype t { source :: a | r } => Lens' t a
_source = _Newtype <<< prop (SProxy :: SProxy "source")

_sinks :: forall t a r. Newtype t { sinks :: a | r } => Lens' t a
_sinks = _Newtype <<< prop (SProxy :: SProxy "sinks")

subtree :: Vector3 -> Number -> List Vector3 -> List (LineSeg Vector3) -> Subtree
subtree source h ss es = Subtree {
    source : source,
    height : h,
    sinks  : ss,
    edges  : es
    }


treeLines :: Subtree -> List (LineSeg Vector3)
treeLines t = mkLineSeg s <$> t ^. _sinks
    where s = t ^. _source

-- get possible gable roof edge
gableEdge :: Subtree -> List Vector3 -> Maybe (Tuple Vector3 Vector3)
gableEdge t vs = toEdge $ foldl f (Tuple 0 Nil) $ t ^. _sinks
    where f (Tuple n ls) v = if elem v vs
                             then Tuple (n + 1) (Cons v ls)
                             else Tuple n ls
          toEdge (Tuple n ls) = if n >= 2
                                then Tuple <$> ls !! 0 <*> ls !! 1
                                else Nothing
