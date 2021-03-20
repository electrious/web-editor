module SmartHouse.Algorithm.LAV where

import Prelude hiding (degree)

import Algorithm.MeshFlatten (_vertex)
import Data.Array (filter, zipWith)
import Data.Array as Arr
import Data.Default (class Default, def)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), fromFoldable, head, singleton, tail, (:))
import Data.Map (lookup, update)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (class Traversable, traverse)
import Data.Triple (Triple(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, emptyUUID, genUUID)
import Data.UUIDMap (UUIDMap)
import Editor.Common.Lenses (_id, _length, _position)
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
    id      :: UUID,
    head    :: Maybe Vertex,
    last    :: Maybe Vertex,
    length  :: Int,

    current :: Maybe Vertex,
    left    :: List Vertex,
    right   :: List Vertex
    }

derive instance newtypeLAV :: Newtype LAV _
derive instance genericLAV :: Generic LAV _
instance showLAV :: Show LAV where
    show = genericShow
instance hasUUIDLAV :: HasUUID LAV where
    idLens = _id
instance defaultLAV :: Default LAV where
    def = LAV {
        id      : emptyUUID,
        head    : Nothing,
        last    : Nothing,
        length  : 0,
        current : Nothing,
        left    : Nil,
        right   : Nil 
        }

_head :: forall t a r. Newtype t { head :: a | r } => Lens' t a
_head = _Newtype <<< prop (SProxy :: SProxy "head")

_last :: forall t a r. Newtype t { last :: a | r } => Lens' t a
_last = _Newtype <<< prop (SProxy :: SProxy "last")

_current :: forall t a r. Newtype t { current :: a | r } => Lens' t a
_current = _Newtype <<< prop (SProxy :: SProxy "current")

_left :: forall t a r. Newtype t { left :: a | r } => Lens' t a
_left = _Newtype <<< prop (SProxy :: SProxy "left")

_right :: forall t a r. Newtype t { right :: a | r } => Lens' t a
_right = _Newtype <<< prop (SProxy :: SProxy "right")

-- create a LAV for a polygon
lavFromPolygon :: forall v. Vector v => Polygon v -> Effect LAV
lavFromPolygon poly = do
    i <- genUUID
    let mkV (Triple prev p next) = vertexFrom p (mkLineSeg prev p) (mkLineSeg p next)
    vs <- traverse mkV $ polyWindows $ getVector <$> poly
    pure $ def # _id      .~ i
               # _head    .~ Arr.head vs
               # _last    .~ Arr.last vs
               # _length  .~ Arr.length vs
               # _current .~ Arr.head vs
               # _right   .~ fromFoldable (fromMaybe [] $ Arr.tail vs)

length :: LAV -> Int
length = view _length

prevVertex :: LAV -> Maybe Vertex
prevVertex lav = case lav ^. _left of
    (l:_) -> Just l
    Nil   -> lav ^. _last

nextVertex :: LAV -> Maybe Vertex
nextVertex lav = case lav ^. _right of
    (r:_) -> Just r
    Nil   -> lav ^. _head

moveRight :: LAV -> LAV
moveRight lav =
    let c = lav ^. _current
        l = lav ^. _left
        nl = maybe l (flip Cons l) c
    in lav # _left    .~ nl
           # _current .~ head (lav ^. _right)
           # _right   %~ (fromMaybe Nil <<< tail)

moveLeft :: LAV -> LAV
moveLeft lav =
    let c = lav ^. _current
        r = lav ^. _right
        nr = maybe r (flip Cons r) c
    in lav # _left    %~ (fromMaybe Nil <<< tail)
           # _current .~ head (lav ^. _left)
           # _right   .~ nr

vertices :: LAV -> Array Vertex
vertices lav = Arr.fromFoldable $ ls <> c <> rs
    where ls = lav ^. _left
          c  = maybe Nil singleton $ lav ^. _current
          rs = lav ^. _right

newtype SLAV = SLAV {
    lavs  :: Array LAV,
    edges :: Array Edge,

    validStates :: UUIDMap Boolean
    }

derive instance newtypeSLAV :: Newtype SLAV _
derive instance genericSLAV :: Generic SLAV _
instance showSLAV :: Show SLAV where
    show = genericShow
instance defaultSLAV :: Default SLAV where
    def = SLAV { lavs : [], edges : [], validStates : M.empty }

_lavs :: forall t a r. Newtype t { lavs :: a | r } => Lens' t a
_lavs = _Newtype <<< prop (SProxy :: SProxy "lavs")

_vertices :: forall t a r. Newtype t { vertices :: a | r } => Lens' t a
_vertices = _Newtype <<< prop (SProxy :: SProxy "vertices")

_edges :: forall t a r. Newtype t { edges :: a | r } => Lens' t a
_edges = _Newtype <<< prop (SProxy :: SProxy "edges")

_validStates :: forall t a r. Newtype t { validStates :: a | r } => Lens' t a
_validStates = _Newtype <<< prop (SProxy :: SProxy "validStates")

-- delete duplicated vertices or connect two consecutive edges if they're in the same direction
normalizeContour :: forall v. Eq v => Vector v => Polygon v -> Polygon v
normalizeContour = newPolygon <<< map g <<< filter f <<< polyWindows
    where f (Triple prev p next) = not $ p == next || normal (p <-> prev) == normal (next <-> p)
          g (Triple _ p _) = p


slavFromPolygon :: forall f v. Functor f => Foldable f => Traversable f => Eq v => Vector v => f (Polygon v) -> Effect SLAV
slavFromPolygon polys = do
    lavs <- Arr.fromFoldable <$> traverse (lavFromPolygon <<< normalizeContour) polys
    let vs    = Arr.concatMap vertices lavs
        ns    = fromMaybe vs $ Arr.snoc <$> Arr.tail vs <*> Arr.head vs
        edges = zipWith f vs ns

        f v n = let vp = v ^. _position
                    np = n ^. _position
                in edge (mkLineSeg vp np) (degree 20.0) (v ^. _bisector) (n ^. _bisector)

    pure $ def # _lavs .~ lavs
               # _edges .~ edges
               # _validStates .~ M.fromFoldable (flip Tuple true <<< view idLens <$> vs)


-- invalidate a vertex in the SLAV
invalidateVertex :: Vertex -> SLAV -> SLAV
invalidateVertex v slav = slav # _validStates %~ update (const $ Just false) (v ^. idLens)

-- check if a vertex is valid or not
isValid :: Vertex -> SLAV -> Boolean
isValid v slav = fromMaybe false $ lookup (v ^. idLens) (slav ^. _validStates)

-- check if an event is valid or not
eventValid :: PointEvent -> SLAV -> Boolean
eventValid (EdgeEvent e) slav  = isValid (e ^. _vertexA) slav && isValid (e ^. _vertexB) slav
eventValid (SplitEvent e) slav = isValid (e ^. _vertex) slav
