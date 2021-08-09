module Smarthouse.Algorithm.Subtree where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_edges, _id)
import Effect (Effect)
import Math.LineSeg (LineSeg, mkLineSeg)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge)
import SmartHouse.Algorithm.VertNode (VertNode)
import Type.Proxy (Proxy(..))

data SubtreeType = NormalNode
                 | MergedNode Edge Edge
                                              -- the subtree node is merged from 3 bisectors.
                                              -- The two edges carried here should be considered to form one edge
                                              -- when generting the roofs.

derive instance genericSubtreeType :: Generic SubtreeType _
derive instance eqSubtreeType :: Eq SubtreeType
instance showSubtreeType :: Show SubtreeType where
    show = genericShow

newtype Subtree = Subtree {
    id              :: UUID,
    source          :: VertNode,
    sinks           :: List VertNode,
    edges           :: List Edge,
    subtreeType     :: SubtreeType
    }

derive instance newtypeSubtree :: Newtype Subtree _
instance eqSubtree :: Eq Subtree where
    eq t1 t2 = t1 ^. idLens == t2 ^. idLens
instance ordSubtree :: Ord Subtree where
    compare = comparing (view idLens)
instance hasUUIDSubtree :: HasUUID Subtree where
    idLens = _id
instance showSubtree :: Show Subtree where
    show t = "Subtree { source: "      <> show (t ^. _source) <>
                     ", sinks: "       <> show (t ^. _sinks) <>
                     ", edges: "       <> show (t ^. _edges) <>
                     ", subtreeType: " <> show (t ^. _subtreeType) <>
                     "}"
    
_source :: forall t a r. Newtype t { source :: a | r } => Lens' t a
_source = _Newtype <<< prop (Proxy :: Proxy "source")

_sinks :: forall t a r. Newtype t { sinks :: a | r } => Lens' t a
_sinks = _Newtype <<< prop (Proxy :: Proxy "sinks")

_subtreeType :: forall t a r. Newtype t { subtreeType :: a | r } => Lens' t a
_subtreeType = _Newtype <<< prop (Proxy :: Proxy "subtreeType")

_isGable :: forall t a r. Newtype t { isGable :: a | r } => Lens' t a
_isGable = _Newtype <<< prop (Proxy :: Proxy "isGable")

_originalSubtree :: forall t a r. Newtype t { originalSubtree :: a | r } => Lens' t a
_originalSubtree = _Newtype <<< prop (Proxy :: Proxy "originalSubtree")


subtree :: SubtreeType -> VertNode -> List VertNode -> List Edge -> Effect Subtree
subtree t source ss es = do
    i <- genUUID
    pure $ Subtree {
        id              : i,
        source          : source,
        sinks           : ss,
        edges           : es,
        subtreeType     : t
        }

normalSubtree :: Subtree -> Boolean
normalSubtree = (==) NormalNode <<< view _subtreeType

-- check if an edge should be merged by the subtree
mergedEdge :: Edge -> Subtree -> Boolean
mergedEdge e t = case t ^. _subtreeType of
    NormalNode -> false
    MergedNode le re -> e == le || e == re

treeLines :: Subtree -> List (LineSeg VertNode)
treeLines t = mkLineSeg s <$> t ^. _sinks
    where s = t ^. _source
