module Model.SmartHouse.House where

import Prelude hiding (degree)

import Control.Alternative (empty)
import Data.Array as Arr
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', set, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, fromFoldable)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Set as S
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, emptyUUID, genUUID)
import Data.UUIDMap (UUIDMap)
import Data.UUIDMap as UM
import Editor.Common.Lenses (_floor, _height, _id, _roofs, _shade, _slope)
import Effect (Effect)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Math.Angle (Angle, degree)
import Model.Polygon (Polygon, _polyVerts, counterClockPoly)
import Model.Roof.RoofPlate (Point, vec2Point)
import Model.SmartHouse.Roof (JSRoof, Roof, RoofState(..), exportRoof, roofState)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Edge (Edge)
import SmartHouse.Algorithm.LAV (_edges, _vertices)
import SmartHouse.Algorithm.Skeleton (skeletonize)
import SmartHouse.Algorithm.VertNode (VertNode)
import SmartHouse.ShadeOption (ShadeOption)
import Smarthouse.Algorithm.RoofGeneration (generateRoofs)
import Smarthouse.Algorithm.Subtree (Subtree, flipSubtree)
import Three.Math.Vector (Vector3)
import Type.Proxy (Proxy(..))

newtype House = House {
    id        :: UUID,
    floor     :: Polygon Vector3,
    height    :: Meter,
    slope     :: Angle,
    trees     :: UUIDMap Subtree,
    vertices  :: UUIDMap VertNode,
    edges     :: List Edge,
    roofs     :: UUIDMap Roof,
    peakPoint :: VertNode
    }

derive instance Newtype House _
derive instance Generic House _
instance Default House where
    def = House {
        id        : emptyUUID,
        floor     : def,
        height    : meter 0.0,
        slope     : degree 20.0,
        trees     : M.empty,
        vertices  : M.empty,
        edges     : empty,
        roofs     : M.empty,
        peakPoint : def
        }
instance Eq House where
    eq h1 h2 = h1 ^. _id == h2 ^. _id
instance Show House where
    show = genericShow
instance HasUUID House where
    idLens = _id

_trees :: forall t a r. Newtype t { trees :: a | r } => Lens' t a
_trees = _Newtype <<< prop (Proxy :: Proxy "trees")

_peakPoint :: forall t a r. Newtype t { peakPoint :: a | r } => Lens' t a
_peakPoint = _Newtype <<< prop (Proxy :: Proxy "peakPoint")

createHouseFrom :: Angle -> Polygon Vector3 -> Effect House
createHouseFrom slope poly = do
    i <- genUUID
    Tuple trees edges <- skeletonize $ counterClockPoly poly
    Tuple roofs nodes <- generateRoofs slope (S.fromFoldable trees) edges

    let n = findPeakPoint nodes
    
    pure $ House {
        id        : i,
        floor     : poly,
        height    : meter 3.5,   -- default height
        slope     : slope,
        trees     : UM.fromFoldable trees,
        vertices  : nodes,
        edges     : edges,
        roofs     : UM.fromFoldable roofs,
        peakPoint : n
        }


-- find the peak point of all subtrees in a house
findPeakPoint :: forall f. Foldable f => f VertNode -> VertNode
findPeakPoint = foldl (\o n -> if n ^. _height > o ^. _height then n else o) def

updateHeight :: Meter -> House -> House
updateHeight height h = h # _height .~ height


updateSlopes :: Angle -> House -> Effect House
updateSlopes slope h = do
    Tuple roofs nodes <- generateRoofs slope (S.fromFoldable (h ^. _trees)) (h ^. _edges)
    pure $ h # _slope    .~ slope
             # _roofs    .~ UM.fromFoldable roofs
             # _vertices .~ nodes

-- load up to date VertNode
getVertNode :: VertNode -> House -> VertNode
getVertNode v h = fromMaybe v $ M.lookup (v ^. idLens) (h ^. _vertices)


-- flip a roof to/from gable
flipRoof :: UUID -> House -> Effect House
flipRoof i h = do
    let vs  = fromFoldable $ h ^. _floor <<< _polyVerts
        ts  = h ^. _trees
        -- flip the subtree at idx
        nts = M.update (Just <<< flip flipSubtree vs) i ts
    -- generate new roofs
    Tuple roofs nodes <- generateRoofs (h ^. _slope) (S.fromFoldable nts) (h ^. _edges)

    pure $ h # _roofs .~ UM.fromFoldable roofs
             # _trees .~ nts
             # _vertices .~ nodes


updateActiveRoofShade :: ShadeOption -> Maybe UUID -> House -> House
updateActiveRoofShade _ Nothing   h = h
updateActiveRoofShade s (Just ai) h = h # _roofs %~ M.update f ai
    where f r = Just $ set _shade s r

exportHouse :: House -> JSHouse
exportHouse h = JSHouse { id: h ^. idLens, floor: floor, height: meterVal $ h ^. _height, roofs: roofs }
    where floor = vec2Point <$> h ^. _floor <<< _polyVerts
          roofs = Arr.fromFoldable $ exportRoof (h ^. _height) <$> filter ((==) SlopeRoof <<< roofState) (h ^. _roofs)

-- House data exported to JSON and saved to server
newtype JSHouse = JSHouse {
    id     :: UUID,
    floor  :: Array Point,
    height :: Number,
    roofs  :: Array JSRoof
    }

derive instance Generic JSHouse _
instance Show JSHouse where
    show = genericShow
instance Encode JSHouse where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance Decode JSHouse where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })


-- all houses generated data to export
newtype JSHouses = JSHouses {
    houses :: Array JSHouse
    }

derive instance Generic JSHouses _
instance Show JSHouses where
    show = genericShow
instance Encode JSHouses where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
