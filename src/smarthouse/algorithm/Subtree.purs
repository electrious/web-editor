module Smarthouse.Algorithm.Subtree where

import Prelude

import Data.Array (foldl)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), elem, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Editor.Common.Lenses (_height)
import Math.LineSeg (LineSeg, mkLineSeg)
import SmartHouse.Algorithm.LAV (_edges)
import Three.Math.Vector (Vector3, (<**>), (<+>))

newtype Subtree = Subtree {
    source          :: Vector3,
    height          :: Number,
    sinks           :: List Vector3,
    edges           :: List (LineSeg Vector3),
    isGable         :: Boolean,         -- if this subtree node is gable
    originalSubtree :: Maybe Subtree    -- the original subtree after gabled
    }

derive instance newtypeSubtree :: Newtype Subtree _
instance showSubtree :: Show Subtree where
    show t = "Subtree { source: "  <> show (t ^. _source) <>
                     ", height: "  <> show (t ^. _height) <>
                     ", sinks: "   <> show (t ^. _sinks) <>
                     ", edges: "   <> show (t ^. _edges) <>
                     ", isGable: " <> show (t ^. _isGable) <>
                     "}"
    
_source :: forall t a r. Newtype t { source :: a | r } => Lens' t a
_source = _Newtype <<< prop (SProxy :: SProxy "source")

_sinks :: forall t a r. Newtype t { sinks :: a | r } => Lens' t a
_sinks = _Newtype <<< prop (SProxy :: SProxy "sinks")

_isGable :: forall t a r. Newtype t { isGable :: a | r } => Lens' t a
_isGable = _Newtype <<< prop (SProxy :: SProxy "isGable")

_originalSubtree :: forall t a r. Newtype t { originalSubtree :: a | r } => Lens' t a
_originalSubtree = _Newtype <<< prop (SProxy :: SProxy "originalSubtree")


subtree :: Vector3 -> Number -> List Vector3 -> List (LineSeg Vector3) -> Subtree
subtree source h ss es = Subtree {
    source          : source,
    height          : h,
    sinks           : ss,
    edges           : es,
    isGable         : false,
    originalSubtree : Nothing
    }


treeLines :: Subtree -> List (LineSeg Vector3)
treeLines t = mkLineSeg s <$> t ^. _sinks
    where s = t ^. _source

gableSubtree :: Subtree -> List Vector3 -> Subtree
gableSubtree t vs = mkT $ foldl f (Tuple 0 Nil) (t ^. _sinks)
    where f (Tuple n ls) v = if elem v vs
                             then Tuple (n + 1) (Cons v ls)
                             else Tuple n ls
          mkT (Tuple n ls) = case ls of
              (v1:v2:_) -> t # _source          .~ (v1 <+> v2) <**> 0.5
                             # _isGable         .~ true
                             # _originalSubtree .~ Just t
              _ -> t


type IndexedSubtree = Tuple Int Subtree

mkIndexedSubtree :: Int -> Subtree -> IndexedSubtree
mkIndexedSubtree = Tuple

getIndex :: IndexedSubtree -> Int
getIndex = fst

getSubtree :: IndexedSubtree -> Subtree
getSubtree = snd
