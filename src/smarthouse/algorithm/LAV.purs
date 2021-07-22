module SmartHouse.Algorithm.LAV where

import Prelude hiding (degree)

import Algorithm.MeshFlatten (_vertex)
import Control.Monad.RWS (get, modify)
import Control.Monad.State (StateT, evalStateT)
import Data.Array (deleteAt, index, last, updateAt, zip, zipWith)
import Data.Array as Arr
import Data.Default (class Default, def)
import Data.Foldable (class Foldable, foldl)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', over, set, view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Map (lookup, update)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Triple (Triple(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, emptyUUID, genUUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_edges, _id, _indices, _vertices)
import Effect (Effect)
import Math.Line (_direction)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge)
import SmartHouse.Algorithm.Event (PointEvent(..), _vertexA, _vertexB, _vertexC)
import SmartHouse.Algorithm.HouseParam (HouseParam)
import SmartHouse.Algorithm.VertInfo (VertInfo, _bisector, _usable)
import SmartHouse.Algorithm.Vertex (Vertex, _lavId, _leftEdge, _rightEdge, vertexFrom, vertexFromVertInfo)
import Three.Math.Vector (Vector3)
import Type.Proxy (Proxy(..))

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
lavFromPolygon :: Array VertInfo -> Array Edge -> Effect LAV
lavFromPolygon vis es = do
    i <- genUUID
    let pes = fromMaybe es $ Arr.cons <$> Arr.last es <*> Arr.init es

        mkV vi (Tuple le re) = vertexFromVertInfo i le re vi
    vs <- sequence $ zipWith mkV vis $ zip pes es
    
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


unifyVerts :: Vertex -> Vertex -> Vector3 -> Number -> LAV -> Effect (Tuple LAV Vertex)
unifyVerts va vb point h lav = do
    nv <- vertexFrom (lav ^. idLens) point h (va ^. _leftEdge) (vb ^. _rightEdge) (Just $ vb ^. _bisector <<< _direction) (Just $ va ^. _bisector <<< _direction)

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


unifyThreeVerts :: Vertex -> Vertex -> Vertex -> Vector3 -> Number -> LAV -> Effect (Triple LAV Vertex Boolean)
unifyThreeVerts va vb vc point h lav = do
    nv <- vertexFrom (lav ^. idLens) point h (va ^. _leftEdge) (vc ^. _rightEdge) (Just $ vc ^. _bisector <<< _direction) (Just $ va ^. _bisector <<< _direction)

    let idxA = vertIndex va lav
        idxB = vertIndex vb lav
        idxC = vertIndex vc lav
        
        vs   = lav ^. _vertices
        om   = lav ^. _indices
        m    = M.delete (va ^. idLens) $ M.delete (vb ^. idLens) $ M.delete (vc ^. idLens) om

    pure $ if nv ^. _usable
           then  -- new lav array after delete va, vb and add nv
               let arr  = join $ (\ia ib ic -> updateAt ia nv vs >>= deleteAt ic >>= deleteAt ib) <$> idxA <*> idxB <*> idxC
        
                   -- update the indices map
                   -- delete 2 for all indices larger than the nv index
                   updIdx nvi i = if nvi == 0
                                  then if i > nvi then i - 1 else i
                                  else if i > nvi then i - 2 else i

                   nm = (\ia ic -> updIdx ic <$> M.insert (nv ^. idLens) ia m) <$> idxA <*> idxC
        
                   newLav = lav # _vertices .~ fromMaybe vs arr
                                # _indices  .~ fromMaybe om nm
               in Triple newLav nv false
           else let arr = join $ (\ia ib ic -> deleteAt ic vs >>= deleteAt ib >>= deleteAt ia) <$> idxA <*> idxB <*> idxC
                    -- delete 3 for all indices larger than the nv index
                    updIdx delta nvi i = if i > nvi then i - delta else i

                    getDelta 0 = 1
                    getDelta 1 = 2
                    getDelta _ = 3

                    nm = (\_ ic -> updIdx (getDelta ic) ic <$> m) <$> idxA <*> idxC
                        
                    newLav = lav # _vertices .~ fromMaybe vs arr
                                 # _indices  .~ fromMaybe om nm
                in Triple newLav nv true


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
_lavs = _Newtype <<< prop (Proxy :: Proxy "lavs")

_validStates :: forall t a r. Newtype t { validStates :: a | r } => Lens' t a
_validStates = _Newtype <<< prop (Proxy :: Proxy "validStates")


type SLAV = StateT SLAVState Effect


slavFrom :: HouseParam -> Effect SLAVState
slavFrom hi = do
    let vis   = hi ^. _vertices
        edges = hi ^. _edges
    lav <- lavFromPolygon vis edges

    let vs = lav ^. _vertices

    pure $ def # _lavs        .~ M.singleton (lav ^. idLens) lav
               # _edges       .~ edges
               # _validStates .~ M.fromFoldable (flip Tuple true <<< view idLens <$> vs)

-- run a SLAV action with a list of polygons
runSLAV :: forall a. SLAV a -> HouseParam -> Effect a
runSLAV slav hi = slavFrom hi >>= evalStateT slav

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

updateLav :: LAV -> Maybe Vertex -> SLAV Unit
updateLav lav (Just v) = void $ modify f
    where f s = s # _lavs        %~ M.update (const $ Just lav) (lav ^. idLens)
                  # _validStates %~ M.insert (v ^. idLens) true
updateLav lav Nothing = void $ modify f
    where f s = s # _lavs %~ M.update (const $ Just lav) (lav ^. idLens)

-- invalidate a vertex in the SLAV
invalidateVertex :: Vertex -> SLAV Unit
invalidateVertex v = void $ modify $ over _validStates $ update (const $ Just false) (v ^. idLens)

-- check if a vertex is valid or not
isValid :: Vertex -> SLAV Boolean
isValid v = get >>= view _validStates >>> lookup (v ^. idLens) >>> fromMaybe false >>> pure

-- check if an event is valid or not
eventValid :: PointEvent -> SLAV Boolean
eventValid (EdgeEvent e)  = (&&) <$> isValid (e ^. _vertexA) <*> isValid (e ^. _vertexB)
eventValid (EdgesEvent e) = f <$> isValid (e ^. _vertexA) <*> isValid (e ^. _vertexB) <*> isValid (e ^. _vertexC)
    where f a b c = a && b && c
eventValid (SplitEvent e) = isValid (e ^. _vertex)

