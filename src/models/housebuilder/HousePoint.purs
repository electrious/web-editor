module Model.HouseBuilder.HousePoint where


import Prelude

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
import Model.Polygon (class PolyVertex)
import Model.UUID (class HasUUID)
import Rendering.Node (mesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Core.Mesh (Mesh)
import Three.Math.Vector (Vector3, multiplyScalar, (<+>))


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
derive instance eqHousePoint :: Eq HousePoint
instance showHousePoint :: Show HousePoint where
    show = genericShow
instance hasUUIDHousePoint :: HasUUID HousePoint where
    idLens = _id
instance defaultHousePoint :: Default HousePoint where
    def = HousePoint {
        id        : emptyUUID,
        position  : def,
        pointType : GutterPoint
        }
instance polyVertexHousePoint :: PolyVertex HousePoint where
    getPos        = view _position
    updatePos     = flip (set  _position)
    addVert p1 p2 = p1 # _position %~ (<+>) (p2 ^. _position)
    scale p n     = p  # _position %~ flip multiplyScalar n
instance nodeRenderableHousePoint :: NodeRenderable e HousePoint Mesh where
    render p = mesh prop gutterGeo gutterMat
        where prop = def # _name     .~ "gutter-point"
                         # _position .~ pure (p ^. _position)

_pointType :: forall t a r. Newtype t { pointType :: a | r } => Lens' t a
_pointType = _Newtype <<< prop (SProxy :: SProxy "pointType")


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
