import {
    Scene,
    PerspectiveCamera,
    WebGLRenderer,
    AmbientLight,
    DirectionalLight
} from 'three'
import { loadHouse } from './house'
import { mkSink } from './sink'
import { setupInput } from './input'
import { newDefaultScheduler } from '@most/scheduler'
import { disposeWith, disposeAll } from '@most/disposable'
import { Scheduler } from '@most/types'
import { tap } from '@most/core'
/**
 * Public Interface for the main WebEditor
 */
export interface WebEditor {
    resize: (width: number, height: number) => void
    dispose: () => void
    loadHouse: (leadId: number) => void
}

/**
 * Interface that defines components for Three.js related objects in the editor.
 */
interface EditorScene {
    scene: Scene
    camera: PerspectiveCamera
    renderer: WebGLRenderer
    render: () => void
    resize: (width: number, height: number) => void
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
    const camera = new PerspectiveCamera(75, width / height, 0.1, 1000)
    const renderer = new WebGLRenderer()

    renderer.setSize(width, height)

    // attach the WebGL canvas to a parent DOM element
    elem.appendChild(renderer.domElement)

    // set the camera position and orient it toward the center
    camera.position.set(0, 0, 80)
    camera.lookAt(0, 0, 0)

    // add ambient light
    const ambLight = new AmbientLight(0xffffff)
    scene.add(ambLight)

    // add a directional light
    const dirLight = new DirectionalLight(0xeeeeee, 0.5)
    dirLight.position.set(100, 0, 100)
    scene.add(dirLight)

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

    /**
     * setup input events for the scene
     */
    const inputEvts = setupInput(elem)
    inputEvts.zoomed.run(
        mkSink(e => {
            let newZ = camera.position.z + e.deltaY
            if (newZ < 5) {
                newZ = 5
            } else if (newZ > 500) {
                newZ = 500
            }
            camera.position.z = newZ
        }),
        scheduler
    )

    return {
        scene: scene,
        camera: camera,
        renderer: renderer,
        render: renderFunc,
        resize: resizeFunc
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
    // create a disposable for the scene
    const sceneDisposable = disposeWith(s => {
        s.dispose()
    }, es.scene)

    // an array of disposables to be disposed at the end of the editor
    const disposables = [sceneDisposable]

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

    const loadHouseFunc = (leadId: number) => {
        const disp = loadHouse(leadId).run(
            mkSink(obj => {
                es.scene.add(obj)
            }),
            scheduler
        )
        disposables.push(disp)
    }

    const editor = {
        resize: es.resize,
        dispose: disposeFunc,
        loadHouse: loadHouseFunc
    }

    return editor
}
