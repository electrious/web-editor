module Model.HouseBuilder.HousePoint where


import Prelude hiding (add,sub)

import Data.Default (class Default, def)
import Data.Filterable (class Filterable, filter)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', set, view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID, emptyUUID, genUUID)
import Editor.Common.Lenses (_id, _name, _position)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Model.UUID (class HasUUID, assignNewId)
import Rendering.Node (mesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Core.Mesh (Mesh)
import Three.Math.Vector (class HasX, class HasY, class Vector, Vector3, add, addScaled, clone, cross, dist, dot, getVector, length, multiplyScalar, normal, sub, updateVector, vecX, vecY)


data HousePointType = GutterPoint
                    | RidgePoint

derive instance eqHousePointType :: Eq HousePointType
derive instance genericHousePointType :: Generic HousePointType _
instance showHousePointType :: Show HousePointType where
    show = genericShow

gutterGeo :: CircleGeometry
gutterGeo = unsafePerformEffect $ mkCircleGeometry 0.2 16

gutterMat :: MeshBasicMaterial
gutterMat = unsafePerformEffect $ mkMeshBasicMaterial 0x00ee00


newtype HousePoint = HousePoint {
    id        :: UUID,
    position  :: Vector3,
    pointType :: HousePointType
    }

derive instance newtypeHousePoint :: Newtype HousePoint _
derive instance genericHousePoint :: Generic HousePoint _
instance eqHousePoint :: Eq HousePoint where
    eq p1 p2 = p1 ^. _id == p2 ^. _id
instance ordHousePoint :: Ord HousePoint where
    compare p1 p2 = (p1 ^. _id) `compare` (p2 ^. _id)
instance showHousePoint :: Show HousePoint where
    show = genericShow
instance hasUUIDHousePoint :: HasUUID HousePoint where
    idLens = _id
instance defaultHousePoint :: Default HousePoint where
    def = HousePoint {
        id        : emptyUUID,
        position  : def,
        pointType : RidgePoint
        }
instance hasXHousePoint :: HasX HousePoint where
    vecX = vecX <<< view _position
instance hasYHousePoint :: HasY HousePoint where
    vecY = vecY <<< view _position
instance vectorHousePoint :: Vector HousePoint where
    dot p1 p2          = (p1 ^. _position) `dot` (p2 ^. _position)
    length p           = length $ p ^. _position
    dist p1 p2         = dist (p1 ^. _position) (p2 ^. _position)
    clone p            = p # _position .~ clone (p ^. _position)
    cross p1 p2        = p1 # _position .~ cross (p1 ^. _position) (p2 ^. _position)
    add p1 p2          = p1 # _position .~ add (p1 ^. _position) (p2 ^. _position)
    addScaled p1 p2 s  = p1 # _position .~ addScaled (p1 ^. _position) (p2 ^. _position) s
    sub p1 p2          = p1 # _position .~ sub (p1 ^. _position) (p2 ^. _position)
    multiplyScalar p s = p # _position %~ flip multiplyScalar s
    normal p           = p # _position %~ normal
    getVector          = view _position
    updateVector       = flip (set  _position)
instance nodeRenderableHousePoint :: NodeRenderable e HousePoint Mesh where
    render p = mesh prop gutterGeo gutterMat
        where prop = def # _name     .~ "gutter-point"
                         # _position .~ pure (p ^. _position)

_pointType :: forall t a r. Newtype t { pointType :: a | r } => Lens' t a
_pointType = _Newtype <<< prop (SProxy :: SProxy "pointType")


funcOver :: forall v. Vector v => (Vector3 -> Vector3 -> Vector3) -> v -> v -> v
funcOver f v1 v2 = updateVector v1 $ f (getVector v1) (getVector v2)

gutterPoints :: forall f. Filterable f => f HousePoint -> f HousePoint
gutterPoints = filter ((==) GutterPoint <<< view _pointType)

ridgePoints :: forall f. Filterable f => f HousePoint -> f HousePoint
ridgePoints = filter ((==) RidgePoint <<< view _pointType)

newHousePoint :: HousePointType -> Vector3 -> Effect HousePoint
newHousePoint t pos = do
    i <- genUUID
    pure $ HousePoint {
        id        : i,
        position  : pos,
        pointType : t
        }

-- merge two HousePoints into one. It will use the second point's position
mergeHousePoint :: HousePoint -> HousePoint -> Effect HousePoint
mergeHousePoint p1 p2 = assignNewId $ p2 # _pointType %~ f (p1 ^. _pointType)
    where f GutterPoint _ = GutterPoint
          f _ GutterPoint = GutterPoint
          f _ _           = RidgePoint
