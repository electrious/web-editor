module SmartHouse.Algorithm.LAV where

import Prelude hiding (degree)

import Algorithm.MeshFlatten (_vertex)
import Control.Monad.RWS (get, modify)
import Control.Monad.State (StateT, evalStateT)
import Data.Array (deleteAt, filter, foldl, index, last, updateAt, zipWith)
import Data.Array as Arr
import Data.Default (class Default, def)
import Data.Foldable (class Foldable)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', over, set, view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Map (lookup, update)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (class Traversable, traverse)
import Data.Triple (Triple(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, emptyUUID, genUUID)
import Data.UUIDMap (UUIDMap)
import Data.UUIDMap as UM
import Editor.Common.Lenses (_id, _indices, _position)
import Effect (Effect)
import Math.Line (_direction)
import Math.LineSeg (mkLineSeg)
import Model.Polygon (Polygon, newPolygon, polyWindows)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge, edge)
import SmartHouse.Algorithm.Event (PointEvent(..), _vertexA, _vertexB)
import SmartHouse.Algorithm.Vertex (Vertex, _bisector, _lavId, _leftEdge, _rightEdge, vertexFrom)
import Three.Math.Vector (class Vector, Vector3, getVector, normal, (<->))

newtype LAV = LAV {
    id       :: UUID,
    vertices :: Array Vertex,
    indices  :: UUIDMap Int
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
        vertices : [],
        indices  : M.empty
        }

-- create a LAV for a polygon
lavFromPolygon :: forall v. Vector v => Polygon v -> Effect LAV
lavFromPolygon poly = do
    i <- genUUID
    let mkV (Triple prev p next) = vertexFrom i p (mkLineSeg prev p) (mkLineSeg p next) Nothing Nothing
    vs <- traverse mkV $ polyWindows $ getVector <$> poly
    let idxMap = M.fromFoldable $ mapWithIndex (\idx v -> Tuple (v ^. idLens) idx) vs
    pure $ def # _id       .~ i
               # _vertices .~ vs
               # _indices  .~ idxMap

lavFromVertices :: forall f. FunctorWithIndex Int f => Foldable f => f Vertex -> Effect LAV
lavFromVertices vs = do
    i <- genUUID
    let nvs = set _lavId i <$> vs
        idxMap = M.fromFoldable $ mapWithIndex (\idx v -> Tuple (v ^. idLens) idx) nvs
    pure $ def # _id       .~ i
               # _vertices .~ Arr.fromFoldable nvs
               # _indices  .~ idxMap

length :: LAV -> Int
length = Arr.length <<< view _vertices

vertIndex :: Vertex -> LAV -> Maybe Int
vertIndex v lav = M.lookup (v ^. idLens) (lav ^. _indices)

prevVertex :: Vertex -> LAV -> Maybe Vertex
prevVertex v lav = vertIndex v lav >>= f
    where f idx = if idx == 0
                  then last $ lav ^. _vertices
                  else index (lav ^. _vertices) (idx - 1)

nextVertex :: Vertex -> LAV -> Maybe Vertex
nextVertex v lav = vertIndex v lav >>= f
    where f idx = if idx == length lav - 1
                  then Arr.head $ lav ^. _vertices
                  else index (lav ^. _vertices) (idx + 1)


-- find all vertices between start and end vertices, both included
verticesFromTo :: Vertex -> Vertex -> LAV -> List Vertex
verticesFromTo vs ve lav = go (Just vs) Nil
    where go (Just v) ls | v == ve   = Cons v ls
                         | otherwise = go (nextVertex v lav) (Cons v ls)
          go Nothing ls = ls


unifyVerts :: Vertex -> Vertex -> Vector3 -> LAV -> Effect (Tuple LAV Vertex)
unifyVerts va vb point lav = do
    nv <- vertexFrom (lav ^. idLens) point (va ^. _leftEdge) (vb ^. _rightEdge) (Just $ vb ^. _bisector <<< _direction) (Just $ va ^. _bisector <<< _direction)

    let idxA = vertIndex va lav
        idxB = vertIndex vb lav
        
        vs   = lav ^. _vertices
        -- new lav array after delete va, vb and add nv
        arr  = join $ (\ia ib -> updateAt ia nv vs >>= deleteAt ib) <$> idxA <*> idxB
        
        -- update the indices map
        om = lav ^. _indices
        m  = M.delete (va ^. idLens) $ M.delete (vb ^. idLens) om
        -- delete 1 for all indices larger than the nv index
        updIdx nvi i = if i > nvi then i - 1 else i

        nm = (\ia ib -> updIdx ib <$> M.insert (nv ^. idLens) ia m) <$> idxA <*> idxB
        
        newLav = lav # _vertices .~ fromMaybe vs arr
                     # _indices  .~ fromMaybe om nm
    pure $ Tuple newLav nv


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
                in edge (mkLineSeg vp np) (v ^. _bisector) (n ^. _bisector)

    pure $ def # _lavs        .~ UM.fromFoldable lavs
               # _edges       .~ edges
               # _validStates .~ M.fromFoldable (flip Tuple true <<< view idLens <$> vs)

-- run a SLAV action with a list of polygons
runSLAV :: forall a f v. Functor f => Foldable f => Traversable f => Eq v => Vector v => SLAV a -> f (Polygon v) -> Effect a
runSLAV slav ps = slavFromPolygon ps >>= evalStateT slav

emptySLAV :: SLAV Boolean
emptySLAV = M.isEmpty <<< view _lavs <$> get

getLav :: UUID -> SLAV (Maybe LAV)
getLav i = M.lookup i <<< view _lavs <$> get

-- add a new LAV to SLAV, with all new vertices nvs's validstate setup correctly
addLav :: LAV -> List Vertex -> SLAV Unit
addLav lav nvs = void $ modify f
    where f s = s # _lavs        %~ M.insert (lav ^. idLens) lav
                  # _validStates %~ flip (foldl g) nvs
          g m v = M.insert (v ^. idLens) true m

delLav :: UUID -> SLAV Unit
delLav i = void $ modify $ over _lavs $ M.delete i

updateLav :: LAV -> Vertex -> SLAV Unit
updateLav lav v = void $ modify f
    where f s = s # _lavs        %~ M.update (const $ Just lav) (lav ^. idLens)
                  # _validStates %~ M.insert (v ^. idLens) true

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