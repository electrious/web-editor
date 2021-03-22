module SmartHouse.Algorithm.LAV where

import Prelude hiding (degree)

import Algorithm.MeshFlatten (_vertex)
import Control.Monad.RWS (get, modify)
import Control.Monad.State (StateT)
import Data.Array (filter, index, last, zipWith)
import Data.Array as Arr
import Data.Default (class Default, def)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', over, view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (lookup, update)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Triple (Triple(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, emptyUUID, genUUID)
import Data.UUIDMap (UUIDMap)
import Data.UUIDMap as UM
import Editor.Common.Lenses (_id, _index, _position)
import Effect (Effect)
import Math.Angle (degree)
import Math.LineSeg (mkLineSeg)
import Model.Polygon (Polygon, newPolygon, polyWindows)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge, edge)
import SmartHouse.Algorithm.Event (PointEvent(..), _vertexA, _vertexB)
import SmartHouse.Algorithm.Vertex (Vertex, _bisector, vertexFrom)
import Three.Math.Vector (class Vector, getVector, normal, (<->))

newtype LAV = LAV {
    id       :: UUID,
    vertices :: Array Vertex
    }

derive instance newtypeLAV :: Newtype LAV _
derive instance genericLAV :: Generic LAV _
instance showLAV :: Show LAV where
    show = genericShow
instance hasUUIDLAV :: HasUUID LAV where
    idLens = _id
instance eqLAV :: Eq LAV where
    eq l1 l2 = l1 ^. idLens == l2 ^. idLens
instance defaultLAV :: Default LAV where
    def = LAV {
        id       : emptyUUID,
        vertices : []
        }

-- create a LAV for a polygon
lavFromPolygon :: forall v. Vector v => Polygon v -> Effect LAV
lavFromPolygon poly = do
    i <- genUUID
    let mkV idx (Triple prev p next) = vertexFrom i idx p (mkLineSeg prev p) (mkLineSeg p next)
    vs <- traverseWithIndex mkV $ polyWindows $ getVector <$> poly
    pure $ def # _id       .~ i
               # _vertices .~ vs

length :: LAV -> Int
length = Arr.length <<< view _vertices

prevVertex :: Vertex -> LAV -> Maybe Vertex
prevVertex v lav = if v ^. _index == 0
                   then last $ lav ^. _vertices
                   else index (lav ^. _vertices) (v ^. _index - 1)

nextVertex :: Vertex -> LAV -> Maybe Vertex
nextVertex v lav = if v ^. _index == length lav - 1
                   then Arr.head $ lav ^. _vertices
                   else index (lav ^. _vertices) (v ^. _index + 1)

newtype SLAVState = SLAVState {
    lavs        :: UUIDMap LAV,
    edges       :: Array Edge,

    validStates :: UUIDMap Boolean
    }

derive instance newtypeSLAV :: Newtype SLAVState _
derive instance genericSLAV :: Generic SLAVState _
instance showSLAV :: Show SLAVState where
    show = genericShow
instance defaultSLAV :: Default SLAVState where
    def = SLAVState { lavs : M.empty, edges : [], validStates : M.empty }

_lavs :: forall t a r. Newtype t { lavs :: a | r } => Lens' t a
_lavs = _Newtype <<< prop (SProxy :: SProxy "lavs")

_vertices :: forall t a r. Newtype t { vertices :: a | r } => Lens' t a
_vertices = _Newtype <<< prop (SProxy :: SProxy "vertices")

_edges :: forall t a r. Newtype t { edges :: a | r } => Lens' t a
_edges = _Newtype <<< prop (SProxy :: SProxy "edges")

_validStates :: forall t a r. Newtype t { validStates :: a | r } => Lens' t a
_validStates = _Newtype <<< prop (SProxy :: SProxy "validStates")


type SLAV = StateT SLAVState Effect

-- delete duplicated vertices or connect two consecutive edges if they're in the same direction
normalizeContour :: forall v. Eq v => Vector v => Polygon v -> Polygon v
normalizeContour = newPolygon <<< map g <<< filter f <<< polyWindows
    where f (Triple prev p next) = not $ p == next || normal (p <-> prev) == normal (next <-> p)
          g (Triple _ p _) = p


slavFromPolygon :: forall f v. Functor f => Foldable f => Traversable f => Eq v => Vector v => f (Polygon v) -> Effect SLAVState
slavFromPolygon polys = do
    lavs <- Arr.fromFoldable <$> traverse (lavFromPolygon <<< normalizeContour) polys
    let vs    = Arr.concatMap (view _vertices) lavs
        ns    = fromMaybe vs $ Arr.snoc <$> Arr.tail vs <*> Arr.head vs
        edges = zipWith f vs ns

        f v n = let vp = v ^. _position
                    np = n ^. _position
                in edge (mkLineSeg vp np) (degree 20.0) (v ^. _bisector) (n ^. _bisector)

    pure $ def # _lavs        .~ UM.fromFoldable lavs
               # _edges       .~ edges
               # _validStates .~ M.fromFoldable (flip Tuple true <<< view idLens <$> vs)

getLav :: UUID -> SLAV (Maybe LAV)
getLav i = M.lookup i <<< view _lavs <$> get

delLav :: UUID -> SLAV Unit
delLav i = void $ modify $ over _lavs $ M.delete i

-- invalidate a vertex in the SLAV
invalidateVertex :: Vertex -> SLAV Unit
invalidateVertex v = void $ modify $ over _validStates $ update (const $ Just false) (v ^. idLens)

-- check if a vertex is valid or not
isValid :: Vertex -> SLAV Boolean
isValid v = get >>= view _validStates >>> lookup (v ^. idLens) >>> fromMaybe false >>> pure

-- check if an event is valid or not
eventValid :: PointEvent -> SLAV Boolean
eventValid (EdgeEvent e)  = (&&) <$> isValid (e ^. _vertexA) <*> isValid (e ^. _vertexB)
eventValid (SplitEvent e) = isValid (e ^. _vertex)
