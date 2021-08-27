module SmartHouse.ChimneyBuilder where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Data.Default (def)
import Data.Lens (set, view, (.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meter, meterVal)
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_face, _height, _isActive, _length, _name, _parent, _point, _position, _scale, _tapped, _updated, _width)
import Editor.ObjectAdder (AdderType(..), createObjectAdder, mkCandidatePoint)
import Editor.SceneEvent (SceneMouseMoveEvent)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, distinctDyn, sampleDyn, step)
import FRP.Event (Event)
import FRP.Event.Extra (multicast, performEvent)
import Model.ActiveMode (ActiveMode, isActive)
import Model.SmartHouse.Chimney (Chimney, ChimneyNode, ChimneyOp(..), chimneyScale, mkChimney)
import Model.UUID (idLens)
import Rendering.Node (Node, _visible, fixNodeDWith, node, tapMesh)
import Three.Core.Face3 (normal)
import Three.Core.Geometry (BoxGeometry, BufferGeometry, CircleGeometry, mkBoxGeometry, mkCircleGeometry)
import Three.Core.Material (MeshPhongMaterial, mkMeshPhongMaterial)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (Vector3, mkVec3, vecX, vecY, vecZ)
import UI.DraggableObject (DragObjCfg, _customGeo, _deltaTransform, _validator, createDraggableObject)


addChimney :: forall e. Dynamic ActiveMode -> Event SceneMouseMoveEvent -> Node e (Event Chimney)
addChimney actDyn mouseEvt = node (def # _name .~ "chimney-builder"
                                       # _visible .~ (isActive <$> actDyn)) do
    e <- ask
    let parent = e ^. _parent

        -- get candidate point
        getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            pure $ Just $ mkCandidatePoint np (normal $ evt ^. _face)
       
        candPntDyn = step Nothing $ performEvent $ getCandPoint <$> mouseEvt

        opt = def # _name .~ "chimney-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.01)
        
    addedPntEvt <- node opt $ createObjectAdder DefaultAdder candPntDyn (isActive <$> actDyn)

    pure $ performEvent $ mkChimney <<< view _position <$> addedPntEvt


chimGeo :: BoxGeometry
chimGeo = unsafePerformEffect $ mkBoxGeometry 1.0 1.0 1.0

chimMat :: MeshPhongMaterial
chimMat = unsafePerformEffect $ mkMeshPhongMaterial 0xb35900

editChimney :: forall e. Chimney -> Dynamic ActiveMode -> Node e ChimneyNode
editChimney chimney actDyn = fixNodeDWith chimney $ \chimDyn -> do
    let posDyn   = distinctDyn $ view _position <$> chimDyn
        scaleDyn = distinctDyn $ chimneyScale <$> chimDyn

        isActDyn = isActive <$> actDyn

    tapEvt <- view _tapped <$> tapMesh (def # _name .~ "tap"
                                            # _position .~ posDyn
                                            # _scale .~ scaleDyn
                                        ) chimGeo chimMat
    
    hEvt <- heightBtn isActDyn chimney posDyn
    Tuple lEvt wEvt <- sizeBtn isActDyn chimney posDyn
    posEvt <- chimPosBtn isActDyn chimney

    let updEvt = (set _height <$> hEvt) <|>
                 (set _length <$> lEvt) <|>
                 (set _width  <$> wEvt) <|>
                 (set _position <$> posEvt)

        chimneyEvt = multicast $ sampleDyn chimDyn updEvt
        opEvt = ChimUpdate <$> chimneyEvt

        cid = chimney ^. idLens
        cn = def # idLens .~ cid
                 # _tapped .~ (const cid <$> tapEvt)
                 # _updated .~ opEvt

    pure { input: chimneyEvt, output: cn }


-- button to change height of the chimney
heightBtn :: forall e. Dynamic Boolean -> Chimney -> Dynamic Vector3 -> Node e (Event Meter)
heightBtn actDyn chimney posDyn = node (def # _name .~ "height-btn"
                                            # _position .~ posDyn) $
    fixNodeDWith (chimney ^. _height) \heightDyn -> do
        let h = meterVal $ chimney ^. _height

            valid v = let z = vecZ v - 0.5
                      in z > 0.5 && z < 50.0

            transZ v = mkVec3 0.0 0.0 (vecZ v)

            cfg :: DragObjCfg BufferGeometry
            cfg = def # _isActive .~ actDyn
                      # _position .~ mkVec3 0.0 0.0 (h + 0.5)
                      # _validator .~ pure valid
                      # _deltaTransform .~ Just transZ
        
        btn <- createDraggableObject cfg
        let hEvt = meter <<< ((-) 0.5) <<< vecZ <$> btn ^. _position

        pure { input : hEvt, output: hEvt }


-- button to change size of the chimney
sizeBtn :: forall e. Dynamic Boolean -> Chimney -> Dynamic Vector3 -> Node e (Tuple (Event Meter) (Event Meter))
sizeBtn actDyn chimney posDyn = node (def # _name .~ "size-btn"
                                          # _position .~ posDyn) $
    fixNodeDWith (chimney ^. _length) \lenDyn ->
        fixNodeDWith (chimney ^. _width) \widDyn -> do
            let toTarget v = mkVec3 (vecX v + 0.5) (vecY v) (vecZ v)
                fromTarget v = mkVec3 (vecX v - 0.5) (vecY v) (vecZ v)

                valid v = let x = vecX v - 0.5
                              y = vecY v - 0.5
                          in x > 0.5 && x < 50.0 && y > 0.5 && y < 50.0
                
                transXY v = mkVec3 (vecX v) (vecY v) 0.0
                
                cfg :: DragObjCfg CircleGeometry
                cfg = def # _isActive .~ actDyn
                          # _position .~ toTarget (mkVec3 0.0 0.0 0.5)
                          # _validator .~ pure valid
                          # _deltaTransform .~ Just transXY
            btn <- createDraggableObject cfg
            let tEvt = multicast $ fromTarget <$> btn ^. _position
                lEvt = meter <<< vecX <$> tEvt
                wEvt = meter <<< vecY <$> tEvt
            pure { input: wEvt, output: { input: lEvt, output: Tuple lEvt wEvt } }


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
