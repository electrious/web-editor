import {
    Scene,
    PerspectiveCamera,
    WebGLRenderer,
    AmbientLight,
    DirectionalLight,
    Object3D,
    Vector3
} from 'three'
import { loadHouse, HouseMeshData, getHouseMeshData } from './house'
import { mkSink } from './sink'
import { setupInput, DragEvent } from './input'
import { disposeWith, disposeAll } from '@most/disposable'
import { Stream, Disposable } from '@most/types'
import { createAdapter } from '@most/adapter'
import { RoofPlate, RoofOperation } from './models/roofplate'
import { setupRaycasting } from './sceneevent'
import { defScheduler } from './helper'
import { createRoofManager } from './roofmanager'

export type Size = [number, number]

/**
 * Public Interface for the main WebEditor
 */
export interface WebEditor {
    resize: (size: Size) => void
    dispose: () => void
    loadHouse: (
        leadId: number,
        roofs: RoofPlate[],
        roofUpdated: (r: RoofOperation) => void
    ) => void
}

/**
 * Interface that defines components for Three.js related objects in the editor.
 */
interface EditorScene {
    scene: Scene
    camera: PerspectiveCamera
    renderer: WebGLRenderer
    size: Stream<[number, number]>
    disposable: Disposable
    render: () => void
    resize: (size: Size) => void
    addContent: (obj: Object3D) => void
}

function updateCameraWithZoom(camera: PerspectiveCamera, zoom: number) {
    const pos = camera.position
    const curDist = pos.length()
    const norm = pos.normalize()

    let newDist = curDist + zoom
    if (newDist < 5) {
        newDist = 5
    } else if (newDist > 500) {
        newDist = 500
    }

    camera.position = norm.multiplyScalar(newDist)
}

function updateContentWithDrag(obj: Object3D, drag: DragEvent) {
    obj.rotateZ(drag.deltaX / 360)
    obj.rotateOnWorldAxis(new Vector3(1, 0, 0), drag.deltaY / 360)
}

/**
 * internal function to create the Threejs scene, camera, light and renderer.
 * @param width
 * @param height
 * @param elem
 */
function createScene(
    width: number,
    height: number,
    elem: Element
): EditorScene {
    const scene = new Scene()
    // create a disposable for the scene
    const sceneDisposable = disposeWith(s => {
        s.dispose()
    }, scene)

    const camera = new PerspectiveCamera(45, width / height, 0.1, 1000)
    const renderer = new WebGLRenderer()

    const [updateSize, sizeStream] = createAdapter()

    const scheduler = defScheduler()

    // function to update camera and renderer when size changed.
    const resized = (s: Size) => {
        const width = s[0]
        const height = s[1]
        camera.aspect = width / height
        renderer.setSize(width, height)
    }
    const disposable0 = sizeStream.run(mkSink(resized), scheduler)

    // attach the WebGL canvas to a parent DOM element
    elem.appendChild(renderer.domElement)

    // set the camera position and orient it toward the center
    camera.position.set(0, -50, 50)
    camera.lookAt(0, 0, 0)

    // add ambient light
    const ambLight = new AmbientLight(0xffffff)
    ambLight.name = 'ambient-light'
    scene.add(ambLight)

    // add a directional light
    const dirLight = new DirectionalLight(0xeeeeee, 0.5)
    dirLight.name = 'directional-light'
    dirLight.position.set(100, 0, 100)
    scene.add(dirLight)

    // add a wrapper object to be parent all user contents
    const content = new Object3D()
    content.name = 'scene-content'
    scene.add(content)

    /**
     * function to update renderring of the WebGL scene
     */
    const renderFunc = () => {
        renderer.render(scene, camera)
    }

    const addContentFunc = (obj: Object3D) => {
        content.add(obj)
    }

    /**
     * setup input events for the scene
     */
    const inputEvts = setupInput(elem)
    const disposable1 = inputEvts.zoomed.run(
        mkSink(e => updateCameraWithZoom(camera, e.deltaY)),
        scheduler
    )

    const rcs = setupRaycasting(camera, scene, inputEvts, sizeStream)

    // use drag events filtered by raycasting already
    const disposable2 = rcs.dragEvent.run(
        mkSink(e => updateContentWithDrag(content, e)),
        scheduler
    )
    // update with the default size
    updateSize([width, height])

    return {
        scene: scene,
        camera: camera,
        renderer: renderer,
        size: sizeStream,
        disposable: disposeAll([
            sceneDisposable,
            disposable0,
            disposable1,
            disposable2,
            rcs.disposable,
            inputEvts.disposable
        ]),
        render: renderFunc,
        resize: updateSize,
        addContent: addContentFunc
    }
}

/**
 * createEditor will create the Web Editor instance
 * @param {Number} width width of the viewport
 * @param {Number} height height of the viewport
 * @param {HTMLElement} dom parent DOM element to attach the WebGL canvas to
 */
export function createEditor(
    width: number,
    height: number,
    elem: Element
): WebEditor {
    // create the editor scene
    const es = createScene(width, height, elem)

    // an array of disposables to be disposed at the end of the editor
    const disposables = [es.disposable]

    const renderLoop = () => {
        requestAnimationFrame(renderLoop)

        es.render()
    }
    // start the renderring
    renderLoop()

    /**
     * dispose all resources used by the editor
     */
    const disposeFunc = () => {
        disposeAll(disposables)
    }

    const loadHouseFunc = (
        leadId: number,
        roofs: RoofPlate[],
        roofUpdated: (r: RoofOperation) => void
    ) => {
        const loadRoofs = (md: HouseMeshData) => {
            // add all roofs to a new roof manager
            const mgr = createRoofManager(md, roofs)
            disposables.push(mgr.disposable)
            disposables.push(
                mgr.roofOps.run(mkSink(roofUpdated), defScheduler())
            )

            es.addContent(mgr.roofWrapper)
        }

        const f = (h: Object3D) => {
            es.addContent(h)
            const md = getHouseMeshData(h)
            if (md != null) loadRoofs(md)
        }

        const disp = loadHouse(leadId).run(mkSink(f), defScheduler())
        disposables.push(disp)
    }

    const editor = {
        resize: es.resize,
        dispose: disposeFunc,
        loadHouse: loadHouseFunc
    }

    return editor
}
