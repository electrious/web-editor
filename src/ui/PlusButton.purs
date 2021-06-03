module Editor.UI.PlusButton where

import Prelude hiding (add)

import Data.Default (def)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_dragged, _name, _position, _rotation, _scale, _tapped)
import Editor.UI.DragInfo (DragInfo, mkDragInfo)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event)
import Model.PlusButton (PlusButton, btnPosition, btnRotation)
import Rendering.Node (Node, _castShadow, _renderOrder, mesh, node, tapDragMesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (BoxGeometry, mkBoxGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial, setOpacity, setTransparent)
import Three.Core.Mesh (Mesh)
import Three.Math.Vector (Vector3, mkVec3)

newtype PlusButtonNode = PlusButtonNode {
    plusButton :: PlusButton,
    tapped     :: Event PlusButton,
    dragged    :: Event (DragInfo PlusButton)
}

derive instance newtypePlusButtonNode :: Newtype PlusButtonNode _

_plusButton :: forall t a r. Newtype t { plusButton :: a | r } => Lens' t a
_plusButton = _Newtype <<< prop (SProxy :: SProxy "plusButton")

plusButtonGeo :: BoxGeometry
plusButtonGeo = unsafePerformEffect $ mkBoxGeometry 1.0 1.0 0.001

whiteMaterial :: MeshBasicMaterial
whiteMaterial = unsafePerformEffect $ mkMeshBasicMaterial 0xffffff

invMaterial :: MeshBasicMaterial
invMaterial = unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xffffff
    setTransparent true mat
    setOpacity 0.0001 mat
    pure mat

mkPlusBtnPart :: forall e. Vector3 -> Vector3 -> Node e Mesh
mkPlusBtnPart scale pos = mesh (def # _scale      .~ pure scale
                                    # _position   .~ pure pos
                                    # _castShadow .~ false
                               ) plusButtonGeo whiteMaterial

{-
moveBy :: Vector3 -> PlusButtonNode -> Effect PlusButtonNode
moveBy delta node = do
    let b = node ^. _button
        pos = position b
        newPos = pos <+> delta
    
    setPosition newPos b
    pure $ node # _plusButton %~ addDelta delta
-}

instance nodeRenderablePlusButtonNode :: NodeRenderable e PlusButton PlusButtonNode where
    render pb = node (def # _name     .~ "PlusButton"
                          # _rotation .~ pure (btnRotation pb)
                          # _position .~ pure (btnPosition pb)
                     ) do
        -- setup left, right, top and bottom parts
        void $ mkPlusBtnPart (mkVec3 0.05 1.0 1.0) (mkVec3 (-0.75) 0.0 0.0)
        void $ mkPlusBtnPart (mkVec3 0.05 1.0 1.0) (mkVec3 0.75 0.0 0.0)
        void $ mkPlusBtnPart (mkVec3 1.5 0.05 1.0) (mkVec3 0.0 0.475 0.0)
        void $ mkPlusBtnPart (mkVec3 1.5 0.05 1.0) (mkVec3 0.0 (-0.475) 0.0)

        -- setup the invisible mesh for tap and drag
        invNode <- tapDragMesh (def # _scale       .~ pure (mkVec3 1.5 1.0 1.0)
                                    # _castShadow  .~ false
                                    # _renderOrder .~ 30
                               ) plusButtonGeo invMaterial
        
        pure $ PlusButtonNode {
            plusButton : pb,
            tapped     : const pb <$> invNode ^. _tapped,
            dragged    : mkDragInfo pb <$> invNode ^. _dragged
        }
