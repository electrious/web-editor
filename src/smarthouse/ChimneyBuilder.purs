module SmartHouse.ChimneyBuilder where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Custom.Mesh (TappableMesh)
import Data.Default (def)
import Data.Lens (addOver, set, view, (.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meter, meterVal)
import Data.Tuple (Tuple(..), fst, snd)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_face, _height, _isActive, _length, _name, _parent, _point, _position, _rotation, _scale, _tapped, _updated, _width)
import Editor.ObjectAdder (AdderType(..), createObjectAdder, mkCandidatePoint)
import Editor.SceneEvent (SceneMouseMoveEvent)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, distinctDyn, sampleDyn, step)
import FRP.Event (Event, sampleOn)
import FRP.Event.Extra (multicast, performEvent)
import Math (abs, pi)
import Math.Angle (Angle, degree, radianVal)
import Model.ActiveMode (ActiveMode, isActive)
import Model.SmartHouse.Chimney (Chimney, ChimneyNode, ChimneyOp(..), mkChimney)
import Model.UUID (idLens)
import Rendering.Node (Node, _exportable, _raycastable, _visible, fixNodeDWith, node, tapMesh)
import SmartHouse.ArrowGeometry (rotateArrowGeo)
import Three.Core.Face3 (normal)
import Three.Core.Geometry (BoxGeometry, BufferGeometry, CircleGeometry, mkBoxGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, MeshPhongMaterial, doubleSide, mkMeshBasicMaterial, mkMeshPhongMaterial, setSide)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Euler (Euler, mkEuler)
import Three.Math.Vector (Vector3, mkVec3, vecX, vecY, vecZ)
import UI.DraggableObject (DragObjCfg, _customGeo, _deltaTransform, _validator, createDraggableObject)


addChimney :: forall e. Dynamic ActiveMode -> Event (Tuple UUID SceneMouseMoveEvent) -> Node e (Event Chimney)
addChimney actDyn mouseEvt = node (def # _name .~ "chimney-builder"
                                       # _visible .~ (isActive <$> actDyn)) do
    e <- ask
    let parent = e ^. _parent

        -- get candidate point
        getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            pure $ Just $ mkCandidatePoint np (normal $ evt ^. _face)
       
        mEvt = multicast mouseEvt
        candPntDyn = step Nothing $ performEvent $ getCandPoint <<< snd <$> mEvt

        opt = def # _name .~ "chimney-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.01)
        
    addedPntEvt <- node opt $ createObjectAdder DefaultAdder candPntDyn (isActive <$> actDyn)
    let posEvt = view _position <$> addedPntEvt
    pure $ performEvent $ sampleOn (fst <$> mEvt) $ flip mkChimney <$> posEvt


chimGeo :: BoxGeometry
chimGeo = unsafePerformEffect $ mkBoxGeometry 1.0 1.0 1.0

chimMat :: MeshPhongMaterial
chimMat = unsafePerformEffect $ mkMeshPhongMaterial 0xb35900

chimBoxPos :: Chimney -> Vector3
chimBoxPos c = mkVec3 (vecX p) (vecY p) ((vecZ p + h) / 2.0)
    where p = c ^. _position
          h = meterVal $ c ^. _height

chimBoxScale :: Chimney -> Vector3
chimBoxScale c = mkVec3 (meterVal $ c ^. _length)
                        (meterVal $ c ^. _width)
                        (h + z)
    where h = meterVal $ c ^. _height
          z = vecZ $ c ^. _position

chimRotation :: Chimney -> Euler
chimRotation c = mkEuler 0.0 0.0 (radianVal $ c ^. _rotation)

editChimney :: forall e. Dynamic ActiveMode -> Chimney -> Node e ChimneyNode
editChimney actDyn chimney = fixNodeDWith chimney $ \chimDyn -> do
    let posDyn   = distinctDyn $ view _position <$> chimDyn

        isActDyn = isActive <$> actDyn

    tapEvt <- view _tapped <$> tapMesh (def # _name .~ "chimney"
                                            # _position .~ (chimBoxPos <$> chimDyn)
                                            # _scale .~ (chimBoxScale <$> chimDyn)
                                            # _rotation .~ (chimRotation <$> chimDyn)
                                            # _exportable .~ true
                                        ) chimGeo chimMat
    
    hEvt <- heightBtn isActDyn chimney posDyn
    Tuple lEvt wEvt <- sizeBtn isActDyn chimney posDyn
    rotEvt <- rotateBtn isActDyn chimDyn posDyn

    let updEvt = (set _height <$> hEvt) <|>
                 (set _length <$> lEvt) <|>
                 (set _width  <$> wEvt) <|>
                 (addOver _rotation <$> rotEvt)

        chimneyEvt = multicast $ sampleDyn chimDyn updEvt
        opEvt = ChimUpdate <$> chimneyEvt

        cid = chimney ^. idLens
        cn = def # idLens .~ cid
                 # _tapped .~ (const cid <$> tapEvt)
                 # _updated .~ opEvt

    pure { input: chimneyEvt, output: cn }


toHeightTarget :: Vector3 -> Vector3
toHeightTarget v = mkVec3 (vecX v) (vecY v) (vecZ v + 1.0)

fromHeightTarget :: Vector3 -> Vector3
fromHeightTarget v = mkVec3 (vecX v) (vecY v) (vecZ v - 1.0)

switchYZ :: Vector3 -> Vector3
switchYZ v = mkVec3 (vecX v) (vecZ v) (vecY v)

-- button to change height of the chimney
heightBtn :: forall e. Dynamic Boolean -> Chimney -> Dynamic Vector3 -> Node e (Event Meter)
heightBtn actDyn chimney posDyn = node (def # _name .~ "height-btn"
                                            # _position .~ posDyn) $ do
    let h = meterVal $ chimney ^. _height

        valid v = let z = vecZ v - 0.5
                  in z > 0.5 && z < 50.0

        transZ v = mkVec3 0.0 0.0 (vecZ v)

        cfg :: DragObjCfg BufferGeometry
        cfg = def # _isActive .~ actDyn
                  # _position .~ switchYZ (toHeightTarget (mkVec3 0.0 0.0 h))
                  # _rotation .~ mkEuler (pi / 2.0) 0.0 0.0
                  # _validator .~ pure valid
                  # _deltaTransform .~ Just transZ

    btn <- createDraggableObject cfg
    pure $ meter <<< vecZ <<< fromHeightTarget <$> btn ^. _position

toSizeTarget :: Vector3 -> Vector3
toSizeTarget v = mkVec3 (vecX v + 0.5) (vecY v) (vecZ v)

fromSizeTarget :: Vector3 -> Vector3
fromSizeTarget v = mkVec3 (vecX v - 0.5) (vecY v) (vecZ v)

-- button to change size of the chimney
sizeBtn :: forall e. Dynamic Boolean -> Chimney -> Dynamic Vector3 -> Node e (Tuple (Event Meter) (Event Meter))
sizeBtn actDyn chimney posDyn = node (def # _name .~ "size-btn"
                                          # _position .~ posDyn) $ do
    let valid v = let x = vecX v - 0.5
                      y = vecY v - 0.5
                  in x > 0.1 && x < 50.0 && y > (-50.0) && y < (-0.1)

        transXY v = mkVec3 (vecX v) (vecY v) 0.0
        
        px = meterVal (chimney ^. _width) / 2.0
        py = meterVal (chimney ^. _length) / (-2.0)

        cfg :: DragObjCfg CircleGeometry
        cfg = def # _isActive .~ actDyn
                  # _position .~ toSizeTarget (mkVec3 px py 0.0)
                  # _validator .~ pure valid
                  # _deltaTransform .~ Just transXY
    btn <- createDraggableObject cfg
    let tEvt = multicast $ fromSizeTarget <$> btn ^. _position
        lEvt = meter <<< (*) 2.0 <<< vecX <$> tEvt
        wEvt = meter <<< (*) 2.0 <<< abs <<< vecY <$> tEvt
    pure $ Tuple lEvt wEvt


rotateArrowMat :: MeshBasicMaterial
rotateArrowMat = unsafePerformEffect do
    m <- mkMeshBasicMaterial 0xff0000
    setSide doubleSide m
    pure m

arrowMesh :: forall e. Node e TappableMesh
arrowMesh = tapMesh (def # _name .~ "arrow"
                         # _position .~ pure (mkVec3 (-0.5) (-0.5) 0.0)
                         # _scale .~ pure (mkVec3 0.005 0.005 1.0)
                    ) rotateArrowGeo rotateArrowMat

-- button to rotate the chimney
rotateBtn :: forall e. Dynamic Boolean -> Dynamic Chimney -> Dynamic Vector3 -> Node e (Event Angle)
rotateBtn actDyn chimney posDyn = node (def # _name .~ "rotate-btn"
                                            # _position .~ posDyn) $ do
    let cwPos c = let px = meterVal (c ^. _width) / (-2.0)
                      py = meterVal (c ^. _length) / (-2.0)
                  in mkVec3 (px - 0.5) py 0.5
        
        ccwPos c = let px = meterVal (c ^. _width) / (-2.0)
                       py = meterVal (c ^. _length) / 2.0
                   in mkVec3 (px - 0.5) (py + 0.5) 0.5

    cwBtn <- node (def # _name .~ "rotate-btn"
                       # _position .~ (cwPos <$> chimney)
                       # _visible .~ actDyn
                       # _raycastable .~ actDyn
                  ) arrowMesh
    
    ccwBtn <- node (def # _name .~ "rotate-btn-2"
                        # _position .~ (ccwPos <$> chimney)
                        # _rotation .~ pure (mkEuler pi 0.0 0.0)
                        # _visible .~ actDyn
                        # _raycastable .~ actDyn) arrowMesh

    pure $ (const (degree 2.0) <$> cwBtn ^. _tapped) <|>
           (const (degree (-2.0)) <$> ccwBtn ^. _tapped)

-- big button to drag the chimney around
posBtnGeo :: CircleGeometry
posBtnGeo = unsafePerformEffect $ mkCircleGeometry 2.0 30

-- button to move chimney
chimPosBtn :: forall e. Dynamic Boolean -> Chimney -> Node e (Event Vector3)
chimPosBtn actDyn chimney = do
    let dragCfg :: DragObjCfg CircleGeometry
        dragCfg = def # _isActive .~ actDyn
                      # _position .~ (chimney ^. _position)
                      # _customGeo .~ Just posBtnGeo
    
    btn <- createDraggableObject dragCfg
    pure $ btn ^. _position
