module Editor.WebEditorScene where

import Prelude hiding (add,degree)

import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.Lens ((^.))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_deltaX, _deltaY, _height, _shiftDragged, _width, _zoomed)
import Editor.Disposable (class Disposable, dispose)
import Editor.EditorMode (EditorMode(..))
import Editor.Input (DragEvent, setupInput)
import Editor.SceneEvent (Size, _dragEvent, setupRaycasting)
import Effect (Effect)
import FRP.Dynamic (Dynamic, gateDyn, subscribeDyn)
import FRP.Event (sampleOn, subscribe)
import FRP.Event.Extra (performEvent)
import Math.Angle (degree, radianVal)
import Three.Controls.OrbitControls (OrbitControls, enableDamping, enableZoom, isEnabled, mkOrbitControls, setAutoRotate, setAutoRotateSpeed, setDampingFactor, setEnabled, setMaxDistance, setMaxPolarAngle, setMinDistance, setMinPolarAngle, setTarget, update)
import Three.Controls.OrbitControls as OrbitControls
import Three.Core.Camera (PerspectiveCamera, Camera, mkPerspectiveCamera, setAspect, updateProjectionMatrix)
import Three.Core.Light (mkAmbientLight, mkDirectionalLight)
import Three.Core.Object3D (Object3D, add, hasParent, lookAt, mkObject3D, parent, position, rotateOnWorldAxis, rotateZ, setDefaultUp, setName, setPosition, translateX, translateY, worldToLocal)
import Three.Core.Scene (disposeScene, mkScene)
import Three.Core.WebGLRenderer (domElement, mkWebGLRenderer, render, setSize)
import Three.Helper.AxesHelper (mkAxesHelper)
import Three.Math.Vector (length, mkVec3, multiplyScalar, normal, vecX, vecY)
import Web.DOM (Element)
import Web.DOM.Element (toNode)
import Web.DOM.Node (appendChild)
import Web.HTML (Window)
import Web.HTML.Window (requestAnimationFrame)
import Web.UIEvent.WheelEvent (deltaY)

-- | internal record that defines all components for threejs related objects
newtype EditorScene a = EditorScene {
    render     :: Effect Unit,
    addContent :: Object3D a -> Effect Unit,
    disposable :: Effect Unit
}

derive instance newtypeEditorScene :: Newtype (EditorScene a) _
instance disposableEditorScene :: Disposable (EditorScene a) where
    dispose (EditorScene { disposable }) = disposable

capVal :: Number -> Number -> Number -> Number
capVal bot top v | v < bot = bot
                 | v > top = top
                 | otherwise = v

setupCameraPos :: forall a. Camera a -> Effect Unit
setupCameraPos camera = do
    setPosition (mkVec3 0.0 (-40.0) 20.0) camera
    lookAt (mkVec3 0.0 0.0 0.0) camera

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
    rotateZ ((drag ^. _deltaX) / 360.0) obj
    rotateOnWorldAxis (mkVec3 1.0 0.0 0.0) ((drag ^. _deltaY) / 360.0) obj

moveWithShiftDrag :: forall a. Object3D a -> DragEvent -> Number -> Effect Unit
moveWithShiftDrag obj drag scale | not (hasParent obj) = pure unit
                                 | otherwise = do
                                    let p = parent obj
                                        vec = mkVec3 (drag ^. _deltaX) (- (drag ^. _deltaY)) 0.0
                                    lVec <- worldToLocal vec p
                                    translateX (vecX lVec * scale / 10.0) obj
                                    translateY (vecY lVec * scale / 10.0) obj

setupOrbitControls :: OrbitControls -> Effect Unit
setupOrbitControls c = do
    setAutoRotate true c
    setAutoRotateSpeed 0.5 c
    enableDamping true c
    setDampingFactor 0.1 c
    enableZoom true c
    setMinPolarAngle (radianVal $ degree 10.0) c
    setMaxPolarAngle (radianVal $ degree 50.0) c
    setMinDistance 15.0 c
    setMaxDistance 35.0 c
    setTarget (mkVec3 0.0 0.0 (-5.0)) c

-- | internal function to create the threejs scene, camera, light and renderer
createScene :: forall a. Dynamic Size -> Dynamic EditorMode -> Element -> Effect (EditorScene a)
createScene sizeDyn modeDyn elem = do
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
    setupOrbitControls orbitCtrl

    let isShowing = (==) Showing <$> modeDyn
        canEdit = not <$> isShowing

    d4 <- subscribeDyn isShowing $ \s -> do
             when (not s) $ setupCameraPos camera
             setEnabled s orbitCtrl

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

    -- function to update renderring of the webgl scene
    let renderFunc = do
            render scene camera renderer
            when (isEnabled orbitCtrl) $ update orbitCtrl

        addContentFunc c = add c content
    
        inputEvts = setupInput elem
    
        newDistEvt = performEvent $ (zoomCamera camera <<< deltaY) <$> (gateDyn canEdit $ inputEvts ^. _zoomed)
        scaleEvt = (\d -> d / cameraDefDist) <$> newDistEvt
    
    rcs <- setupRaycasting camera scene inputEvts sizeDyn

    d2 <- subscribe (gateDyn canEdit $ rcs ^. _dragEvent) (rotateContentWithDrag rotWrapper)

    let shiftDragEvt = performEvent $ sampleOn scaleEvt $ moveWithShiftDrag content <$> gateDyn canEdit (inputEvts ^. _shiftDragged) 
    d3 <- subscribe shiftDragEvt (const $ pure unit)

    pure $ EditorScene {
        render     : renderFunc,
        addContent : addContentFunc,
        disposable : sequence_ [d1, d2, d3, d4, disposeScene scene, dispose rcs, OrbitControls.dispose orbitCtrl]
    }

-- | renderLoop is the function to render scene repeatedly
renderLoop :: Effect Unit -> Window -> Effect Unit
renderLoop render w = do
    _ <- requestAnimationFrame (renderLoop render w) w
    render