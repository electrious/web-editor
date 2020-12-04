module Model.Polygon where

import Prelude

import Control.Monad.Writer (tell)
import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Default (def)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Data.Tuple (snd)
import Editor.Common.Lenses (_mesh)
import Editor.Disposable (Disposee(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, current, subscribeDyn)
import Rendering.Node (getEnv, leaf, tapMesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (mkShape, mkShapeGeometry)
import Three.Core.Material (MeshBasicMaterial)
import Three.Core.Mesh (setMaterial)
import Three.Math.Vector (Vector2)

newtype Polygon = Polygon (Array Vector2)

derive instance newtypePolygon :: Newtype Polygon _
derive instance eqPolygon :: Eq Polygon

_polyVerts :: Lens' Polygon (Array Vector2)
_polyVerts = _Newtype


instance nodeRenderablePolygon :: NodeRenderable (Dynamic MeshBasicMaterial) Polygon TappableMesh where
    render p = do
        -- get the current material used
        matDyn <- getEnv
        mat <- liftEffect $ current matDyn
        
        shp <- liftEffect $ mkShape $ p ^. _polyVerts
        geo <- liftEffect $ mkShapeGeometry shp
        
        m <- snd <$> tapMesh def geo mat leaf

        -- update the material when it changes
        let mesh = m ^. _mesh
        d <- liftEffect $ subscribeDyn matDyn (flip setMaterial mesh)
        tell $ Disposee d

        pure m


renderPolygon :: Polygon -> MeshBasicMaterial -> Effect TappableMesh
renderPolygon p mat = do
    shp <- mkShape $ p ^. _polyVerts
    geo <- mkShapeGeometry shp
    mkTappableMesh geo mat
