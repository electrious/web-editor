import {
    Scene,
    PerspectiveCamera,
    WebGLRenderer,
    AmbientLight,
    DirectionalLight,
    Object3D,
    Vector3
} from 'three'
import { loadHouse } from './house'
import { mkSink } from './sink'
import { setupInput, DragEvent } from './input'
import { newDefaultScheduler } from '@most/scheduler'
import { disposeWith, disposeAll } from '@most/disposable'
import { Scheduler, Disposable } from '@most/types'
import { RoofPlate } from './models/roofplate'
import { createRoofNode } from './roofnode'
import { setupRaycasting } from './sceneevent'

/**
 * Public Interface for the main WebEditor
 */
export interface WebEditor {
    resize: (width: number, height: number) => void
    dispose: () => void
    loadHouse: (leadId: number, roofs: RoofPlate[]) => void
}

/**
 * Interface that defines components for Three.js related objects in the editor.
 */
interface EditorScene {
    scene: Scene
    camera: PerspectiveCamera
    renderer: WebGLRenderer
    disposable: Disposable
    render: () => void
    resize: (width: number, height: number) => void
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
    elem: Element,
    scheduler: Scheduler
): EditorScene {
    const scene = new Scene()
    // create a disposable for the scene
    const sceneDisposable = disposeWith(s => {
        s.dispose()
    }, scene)

    const camera = new PerspectiveCamera(45, width / height, 0.1, 1000)
    const renderer = new WebGLRenderer()

    renderer.setSize(width, height)

    // attach the WebGL canvas to a parent DOM element
    elem.appendChild(renderer.domElement)

    // set the camera position and orient it toward the center
    camera.position.set(0, -50, 50)
    camera.lookAt(0, 0, 0)

    // add ambient light
    const ambLight = new AmbientLight(0xffffff)
    scene.add(ambLight)

    // add a directional light
    const dirLight = new DirectionalLight(0xeeeeee, 0.5)
    dirLight.position.set(100, 0, 100)
    scene.add(dirLight)

    // add a wrapper object to be parent all user contents
    const content = new Object3D()
    scene.add(content)

    /**
     * function to update renderring of the WebGL scene
     */
    const renderFunc = () => {
        renderer.render(scene, camera)
    }

    /**
     * resize the editor viewport
     * @param {Number} width new width of the viewport
     * @param {Number} height new height of the viewport
     */
    const resizeFunc = (width: number, height: number) => {
        camera.aspect = width / height
        renderer.setSize(width, height)
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
    const disposable2 = inputEvts.dragged.run(
        mkSink(e => updateContentWithDrag(content, e)),
        scheduler
    )
    const disposable3 = setupRaycasting(camera, scene, inputEvts, scheduler)

    return {
        scene: scene,
        camera: camera,
        renderer: renderer,
        disposable: disposeAll([
            sceneDisposable,
            disposable1,
            disposable2,
            disposable3
        ]),
        render: renderFunc,
        resize: resizeFunc,
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
    // default scheduler for all event streams
    const scheduler = newDefaultScheduler()

    // create the editor scene
    const es = createScene(width, height, elem, scheduler)

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

    const loadHouseFunc = (leadId: number, roofs: RoofPlate[]) => {
        const f = (h: Object3D) => {
            es.addContent(h)

            roofs.map(createRoofNode).forEach(n => {
                es.addContent(n.roofObject)
            })
        }

        const disp = loadHouse(leadId).run(mkSink(f), scheduler)
        disposables.push(disp)
    }

    const editor = {
        resize: es.resize,
        dispose: disposeFunc,
        loadHouse: loadHouseFunc
    }

    return editor
}
