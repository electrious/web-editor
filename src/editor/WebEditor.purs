module Editor.WebEditor where

import Prelude hiding (add,degree)

import Data.Array (cons)
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_deltaX, _deltaY, _height, _shiftDragged, _width, _zoomed)
import Editor.Disposable (class Disposable, dispose)
import Editor.EditorMode (EditorMode(..))
import Editor.Input (setupInput)
import Editor.Input.Commoon (DragEvent)
import Editor.SceneEvent (Size, _dragEvent, setupRaycasting)
import Effect (Effect)
import Effect.Ref (Ref, modify, new, read)
import FRP.Dynamic (Dynamic, gateDyn, sampleDyn, step, subscribeDyn)
import FRP.Event (subscribe)
import FRP.Event.Extra (performEvent)
import Math.Angle (degree, radianVal)
import Three.Controls.OrbitControls (OrbitControls, enableDamping, enableZoom, isEnabled, mkOrbitControls, setAutoRotate, setAutoRotateSpeed, setDampingFactor, setEnabled, setMaxDistance, setMaxPolarAngle, setMinDistance, setMinPolarAngle, setTarget, update)
import Three.Controls.OrbitControls as OrbitControls
import Three.Core.Camera (PerspectiveCamera, Camera, mkPerspectiveCamera, setAspect, updateProjectionMatrix)
import Three.Core.Light (mkAmbientLight, mkDirectionalLight)
import Three.Core.Object3D (Object3D, add, hasParent, lookAt, mkObject3D, parent, position, rotateOnWorldAxis, rotateZ, setDefaultUp, setName, setPosition, setRotation, translateX, translateY, worldToLocal)
import Three.Core.Scene (disposeScene, mkScene)
import Three.Core.WebGLRenderer (domElement, mkWebGLRenderer, render, setSize)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (Vector3, length, mkVec3, multiplyScalar, normal, vecX, vecY)
import Web.DOM (Element)
import Web.DOM.Element (toNode)
import Web.DOM.Node (appendChild)
import Web.HTML (Window)
import Web.HTML.Window (requestAnimationFrame)

-- | internal record that defines all components for threejs related objects
newtype WebEditor a = WebEditor {
    render     :: Effect Unit,
    addContent :: Object3D a -> Effect Unit,
    disposable :: Ref (Array (Effect Unit))
}

derive instance newtypeEditorScene :: Newtype (WebEditor a) _
instance disposableEditorScene :: Disposable (WebEditor a) where
    dispose (WebEditor { disposable }) = read disposable >>= sequence_

addToScene :: forall a. Object3D a -> WebEditor a -> Effect Unit
addToScene obj (WebEditor s) = s.addContent obj

renderScene :: forall a. WebEditor a -> Effect Unit
renderScene (WebEditor s) = s.render

addDisposable :: forall a. Effect Unit -> WebEditor a -> Effect Unit
addDisposable d (WebEditor e) = void $ modify (cons d) e.disposable

capVal :: Number -> Number -> Number -> Number
capVal bot top v | v < bot = bot
                 | v > top = top
                 | otherwise = v

setupCameraPos :: forall a. Camera a -> Effect Unit
setupCameraPos camera = do
    setPosition (mkVec3 0.0 (-40.0) 20.0) camera
    lookAt (mkVec3 0.0 0.0 0.0) camera

resetContentPos :: forall a. Object3D a -> Effect Unit
resetContentPos = setPosition (mkVec3 0.0 0.0 0.0)

resetContentRot :: forall a. Object3D a -> Effect Unit
resetContentRot = setRotation (mkEuler 0.0 0.0 0.0)

zoomCamera :: forall a. PerspectiveCamera a -> Number -> Effect Number
zoomCamera camera zoom = do
    let pos = position camera
        curDist = length pos
        newDist = capVal 5.0 500.0 (curDist + zoom)
        newPos = multiplyScalar (normal pos) newDist
    setPosition newPos camera
    pure newDist


rotateContentWithDrag :: forall a. Object3D a -> DragEvent -> Effect Unit
rotateContentWithDrag obj drag = do
    let dx = drag ^. _deltaX / 360.0
    rotateZ dx obj
    rotateOnWorldAxis (mkVec3 1.0 0.0 0.0) ((drag ^. _deltaY) / 360.0) obj

moveWithShiftDrag :: forall a. Object3D a -> DragEvent -> Number -> Effect Unit
moveWithShiftDrag obj drag scale | not (hasParent obj) = pure unit
                                 | otherwise = do
                                    let p = parent obj
                                        vec = mkVec3 (drag ^. _deltaX) (- (drag ^. _deltaY)) 0.0
                                    lVec <- worldToLocal vec p
                                    translateX (vecX lVec * scale / 10.0) obj
                                    translateY (vecY lVec * scale / 10.0) obj

setupOrbitControls :: OrbitControls -> Dynamic (Maybe Vector3) -> Effect (Effect Unit)
setupOrbitControls c target = do
    setAutoRotate true c
    setAutoRotateSpeed 0.5 c
    enableDamping true c
    setDampingFactor 0.1 c
    enableZoom true c
    setMinPolarAngle (radianVal $ degree 10.0) c
    setMaxPolarAngle (radianVal $ degree 50.0) c
    setMinDistance 15.0 c
    setMaxDistance 35.0 c
    let t = fromMaybe (mkVec3 0.0 0.0 (-5.0)) <$> target
    subscribeDyn t (flip setTarget c)

-- | internal function to create the threejs scene, camera, light and renderer
createScene :: forall a. Dynamic Size -> Dynamic EditorMode -> Dynamic (Maybe Vector3) -> Element -> Effect (WebEditor a)
createScene sizeDyn modeDyn targetDyn elem = do
    -- set the default Up direction as z axis in the scene
    setDefaultUp (mkVec3 0.0 0.0 1.0)

    scene    <- mkScene
    camera   <- mkPerspectiveCamera 45.0 (800.0 / 600.0) 0.1 1000.0
    renderer <- mkWebGLRenderer

    -- function to update camera and renderer when resized
    let resized s = do
            setAspect (toNumber (s ^. _width) / toNumber (s ^. _height)) camera
            updateProjectionMatrix camera
            setSize (s ^. _width) (s ^. _height) renderer
    
    d1 <- subscribeDyn sizeDyn resized

    -- attach the webgl canvas to parent DOM element
    _ <- appendChild (toNode $ domElement renderer) (toNode elem)

    -- set the camera position and orient it toward the center
    setupCameraPos camera

    let cameraDefDist = length (position camera)

    -- setup the orbit controls
    orbitCtrl <- mkOrbitControls camera (domElement renderer)
    d5 <- setupOrbitControls orbitCtrl targetDyn

    let isShowing = (==) Showing <$> modeDyn
        canEdit = not <$> isShowing

    -- add ambient light
    ambientLight <- mkAmbientLight 0xffffff
    setName "ambient-light" ambientLight
    add ambientLight scene

    -- add a directional light
    dirLight <- mkDirectionalLight 0xeeeeee 0.5
    setName "directional-light" dirLight
    setPosition (mkVec3 100.0 0.0 100.0) dirLight
    add dirLight scene

    -- add a wrapper object that's used only to rotate the scene around
    rotWrapper <- mkObject3D
    setName "rotate-wrapper" rotWrapper
    add rotWrapper scene

    -- add a wrapper object to be parent of all user contents
    content <- mkObject3D
    setName "scene-content" content
    add content rotWrapper

    d4 <- subscribeDyn isShowing $ \s -> do
             if s
                then resetContentPos content *> resetContentRot rotWrapper
                else setupCameraPos camera
             setEnabled s orbitCtrl

    -- function to update renderring of the webgl scene
    let renderFunc = do
            render scene camera renderer
            when (isEnabled orbitCtrl) $ update orbitCtrl

        addContentFunc c = add c content
    
        inputEvts = setupInput (domElement renderer)
    
        newDistEvt = performEvent $ zoomCamera camera <$> (gateDyn canEdit $ inputEvts ^. _zoomed)
        scaleEvt = (\d -> d / cameraDefDist) <$> newDistEvt
        scaleDyn = step 1.0 scaleEvt
    rcs <- setupRaycasting camera scene inputEvts sizeDyn

   
    d2 <- subscribe (gateDyn canEdit (rcs ^. _dragEvent)) (rotateContentWithDrag rotWrapper)

    let shiftDragEvt = performEvent $ sampleDyn scaleDyn $ moveWithShiftDrag content <$> gateDyn canEdit (inputEvts ^. _shiftDragged) 
    d3 <- subscribe shiftDragEvt (const $ pure unit)

    disposable <- new [d1, d2, d3, d4, d5, disposeScene scene, dispose rcs, OrbitControls.dispose orbitCtrl]
    pure $ WebEditor {
        render     : renderFunc,
        addContent : addContentFunc,
        disposable : disposable
    }

-- | renderLoop is the function to render scene repeatedly
renderLoop :: forall a. WebEditor a -> Window -> Effect Unit
renderLoop scene w = do
    _ <- requestAnimationFrame (renderLoop scene w) w
    renderScene scene