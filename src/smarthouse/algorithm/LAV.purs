module SmartHouse.Algorithm.LAV where

import Prelude hiding (degree)

import Data.Array (concatMap, filter, fromFoldable, head, mapWithIndex, snoc, tail, zipWith)
import Data.Default (class Default, def)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Triple (Triple(..))
import Editor.Common.Lenses (_position)
import Math.Angle (degree)
import Math.Line (mkLine)
import Model.Polygon (Polygon, newPolygon, polyWindows)
import SmartHouse.Algorithm.Edge (Edge, edge)
import SmartHouse.Algorithm.Vertex (Vertex, _bisector, vertexFrom)
import Three.Math.Vector (class Vector, getVector, normal, (<->))

newtype LAV = LAV {
    vertices :: Array Vertex
    }

derive instance newtypeLAV :: Newtype LAV _
derive instance genericLAV :: Generic LAV _
instance showLAV :: Show LAV where
    show = genericShow
instance defaultLAV :: Default LAV where
    def = LAV { vertices : [] }

-- create a LAV for a polygon
lavFromPolygon :: forall v. Vector v => Polygon v -> LAV
lavFromPolygon poly = def # _vertices .~ (mapWithIndex mkV $ polyWindows $ getVector <$> poly)
    where mkV idx (Triple prev p next) = vertexFrom idx p (mkLine prev p) (mkLine p next)

newtype SLAV = SLAV {
    lavs  :: Array LAV,
    edges :: Array Edge
    }

derive instance newtypeSLAV :: Newtype SLAV _
derive instance genericSLAV :: Generic SLAV _
instance showSLAV :: Show SLAV where
    show = genericShow
instance defaultSLAV :: Default SLAV where
    def = SLAV { lavs : [], edges : [] }

_lavs :: forall t a r. Newtype t { lavs :: a | r } => Lens' t a
_lavs = _Newtype <<< prop (SProxy :: SProxy "lavs")

_vertices :: forall t a r. Newtype t { vertices :: a | r } => Lens' t a
_vertices = _Newtype <<< prop (SProxy :: SProxy "vertices")

_edges :: forall t a r. Newtype t { edges :: a | r } => Lens' t a
_edges = _Newtype <<< prop (SProxy :: SProxy "edges")


-- delete duplicated vertices or connect two consecutive edges if they're in the same direction
normalizeContour :: forall v. Eq v => Vector v => Polygon v -> Polygon v
normalizeContour = newPolygon <<< map g <<< filter f <<< polyWindows
    where f (Triple prev p next) = not $ p == next || normal (p <-> prev) == normal (next <-> p)
          g (Triple _ p _) = p


slavFromPolygon :: forall f v. Functor f => Foldable f => Eq v => Vector v => f (Polygon v) -> SLAV
slavFromPolygon polys = def # _lavs .~ lavs
                            # _edges .~ edges
    where lavs = fromFoldable (lavFromPolygon <<< normalizeContour <$> polys)
          vs   = concatMap (view _vertices) lavs
          ns   = fromMaybe vs $ snoc <$> tail vs <*> head vs
          edges = zipWith f vs ns

          f v n = let vp = v ^. _position
                      np = n ^. _position
                  in edge (mkLine vp np) (degree 20.0) (v ^. _bisector) (n ^. _bisector)
