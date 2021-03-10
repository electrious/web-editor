module SmartHouse.Algorithm.LAV where

import Control.Category ((<<<))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Show (class Show)
import Data.Symbol (SProxy(..))
import Math.Line (Line)
import SmartHouse.Algorithm.Edge (Edge)
import SmartHouse.Algorithm.Vertex (Vertex)
import Three.Math.Vector (Vector3)

newtype LAV = LAV {
    vertices :: Array Vertex,
    edges    :: Array (Line Vector3)
    }

derive instance newtypeLAV :: Newtype LAV _
derive instance genericLAV :: Generic LAV _
instance showLAV :: Show LAV where
    show = genericShow

newtype SLAV = SLAV {
    lavs  :: Set LAV,
    edges :: Array Edge
    }

derive instance newtypeSLAV :: Newtype SLAV _
derive instance genericSLAV :: Generic SLAV _
instance showSLAV :: Show SLAV where
    show = genericShow

_lavs :: forall t a r. Newtype t { lavs :: a | r } => Lens' t a
_lavs = _Newtype <<< prop (SProxy :: SProxy "lavs")

_vertices :: forall t a r. Newtype t { vertices :: a | r } => Lens' t a
_vertices = _Newtype <<< prop (SProxy :: SProxy "vertices")

_edges :: forall t a r. Newtype t { edges :: a | r } => Lens' t a
_edges = _Newtype <<< prop (SProxy :: SProxy "edges")
