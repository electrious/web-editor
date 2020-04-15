module Editor.Editor (WebEditor, createEditor) where

import Prelude hiding (add)

import Data.Array (cons)
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Editor.House (loadHouse)
import Editor.Input (DragEvent, setupInput)
import Editor.RoofManager (createRoofManager)
import Editor.SceneEvent (Size, setupRaycasting)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, create, sampleOn, subscribe)
import Models.RoofPlate (JSRoofPlate, RoofEdited, fromJSRoofPlate)
import Three.Core.Camera (PerspectiveCamera, mkPerspectiveCamera, setAspect, updateProjectionMatrix)
import Three.Core.Light (mkAmbientLight, mkDirectionalLight)
import Three.Core.Object3D (Object3D, add, hasParent, lookAt, mkObject3D, parent, position, rotateOnWorldAxis, rotateZ, setName, setPosition, translateX, translateY, worldToLocal)
import Three.Core.Scene (Scene, disposeScene, mkScene)
import Three.Core.WebGLRenderer (WebGLRenderer, domElement, mkWebGLRenderer, render, setSize)
import Three.Math.Vector (length, mkVec3, multiplyScalar, normal, vecX, vecY)
import FRP.Event.Extra (performEvent)
import Web.DOM (Element)
import Web.DOM.Element (toNode)
import Web.DOM.Node (appendChild)
import Web.HTML (Window, window)
import Web.HTML.Window (requestAnimationFrame)
import Web.UIEvent.WheelEvent (deltaY)

-- | Public interface for the main WebEditor
type WebEditor = {
    resize    :: Int -> Int -> Effect Unit,
    dispose   :: Effect Unit,
    loadHouse :: String -> Int -> Array JSRoofPlate -> (Array RoofEdited -> Effect Unit) -> Effect Unit
}

-- | internal record that defines all components for threejs related objects
type EditorScene a = {
    scene      :: Scene Unit,
    camera     :: PerspectiveCamera Unit,
    renderer   :: WebGLRenderer,
    size       :: Event Size,
    render     :: Effect Unit,
    resize     :: Int -> Int -> Effect Unit,
    addContent :: Object3D a -> Effect Unit,
    dispose    :: Effect Unit
}

capVal :: Number -> Number -> Number -> Number
capVal bot top v | v < bot = bot
                 | v > top = top
                 | otherwise = v

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
    rotateZ (drag.deltaX / 360.0) obj
    rotateOnWorldAxis (mkVec3 1.0 0.0 0.0) (drag.deltaY / 360.0) obj

moveWithShiftDrag :: forall a. Object3D a -> DragEvent -> Number -> Effect Unit
moveWithShiftDrag obj drag scale | not (hasParent obj) = pure unit
                                 | otherwise = do
                                    let p = parent obj
                                        vec = mkVec3 drag.deltaX (- drag.deltaY) 0.0
                                    lVec <- worldToLocal vec p
                                    translateX (vecX lVec * scale / 10.0) obj
                                    translateY (vecY lVec * scale / 10.0) obj

-- | internal function to create the threejs scene, camera, light and renderer
createScene :: forall a. Int -> Int -> Element -> Effect (EditorScene a)
createScene width height elem = do
    scene <- mkScene
    camera <- mkPerspectiveCamera 45.0 (toNumber width / toNumber height) 0.1 1000.0
    renderer <- mkWebGLRenderer

    -- function to update camera and renderer when resized
    let resized s = do
            setAspect (toNumber s.width / toNumber s.height) camera
            updateProjectionMatrix camera
            setSize s.width s.height renderer
    
    { event: sizeEvt, push: updateSize } <- create
    d1 <- subscribe sizeEvt resized

    -- attach the webgl canvas to parent DOM element
    _ <- appendChild (toNode $ domElement renderer) (toNode elem)

    -- set the camera position and orient it toward the center
    setPosition (mkVec3 0.0 (-50.0) 50.0) camera
    lookAt (mkVec3 0.0 0.0 0.0) camera

    let cameraDefDist = length (position camera)

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
    let renderFunc = render scene camera renderer

        addContentFunc c = add c content
    
        inputEvts = setupInput elem
    
        newDistEvt = performEvent $ (zoomCamera camera <<< deltaY) <$> inputEvts.zoomed
        scaleEvt = (\d -> d / cameraDefDist) <$> newDistEvt
    
    rcs <- setupRaycasting camera scene inputEvts sizeEvt

    d2 <- subscribe rcs.dragEvent (rotateContentWithDrag rotWrapper)

    let shiftDragEvt = performEvent $ sampleOn scaleEvt (moveWithShiftDrag content <$> inputEvts.shiftDragged) 
    d3 <- subscribe shiftDragEvt (\_ -> pure unit)

    updateSize({ width: width, height: height})

    pure {
        scene: scene,
        camera: camera,
        renderer: renderer,
        size: sizeEvt,
        render: renderFunc,
        resize: \w h -> updateSize({ width: w, height: h }),
        addContent: addContentFunc,
        dispose: sequence_ [d1, d2, d3, disposeScene scene, rcs.dispose]
    }

-- | renderLoop is the function to render scene repeatedly
renderLoop :: forall a. EditorScene a -> Window -> Effect Unit
renderLoop es w = do
    _ <- requestAnimationFrame (renderLoop es w) w
    es.render

-- | createEditor will create the Web Editor instance
createEditor :: Int -> Int -> Element -> Effect WebEditor
createEditor width height elem = do
    es <- createScene width height elem
    w <- window

    disposables <- Ref.new [es.dispose]
    
    let addDisposable d = Ref.modify (cons d) disposables

    -- start the rednerring
    renderLoop es w

    let loadHouseFunc url leadId roofs roofEdited = do
            let f hmd = do
                    es.addContent hmd.wrapper
                    mgr <- createRoofManager hmd (fromJSRoofPlate <$> roofs)
                    es.addContent mgr.roofWrapper
                    _ <- addDisposable mgr.disposable
                    d <- subscribe mgr.editedRoofs roofEdited
                    addDisposable d
            e <- loadHouse url leadId
            d <- subscribe e f
            _ <- addDisposable d
            pure unit
        
        disposeFunc = do
            l <- Ref.read disposables
            sequence_ l
            Ref.write [] disposables
    
    pure {
        resize: es.resize,
        dispose: disposeFunc,
        loadHouse: loadHouseFunc
    }