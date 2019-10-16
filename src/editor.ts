import {
    Scene,
    PerspectiveCamera,
    WebGLRenderer,
    AmbientLight,
    DirectionalLight
} from 'three'
import { loadHouse } from './house'
import { newDefaultScheduler } from '@most/scheduler'

export interface WebEditor {
    resize: (width: number, height: number) => void
    dispose: () => void
    loadHouse: (leadId: number) => void
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
    dom: Element
): WebEditor {
    const scene = new Scene()
    const camera = new PerspectiveCamera(75, width / height, 0.1, 1000)
    const renderer = new WebGLRenderer()

    renderer.setSize(width, height)

    // attach the WebGL canvas to a parent DOM element
    dom.appendChild(renderer.domElement)

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

    const animate = () => {
        requestAnimationFrame(animate)

        renderFunc()
    }
    // start the renderring
    animate()

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
     * dispose all resources used by the editor
     */
    const disposeFunc = () => {
        scene.dispose()
    }

    const loadHouseFunc = (leadId: number) => {
        loadHouse(leadId).run(
            {
                event: (t, obj) => {
                    scene.add(obj)
                },
                error: (t, e) => {},
                end: t => {}
            },
            newDefaultScheduler()
        )
    }

    const editor = {
        resize: resizeFunc,
        dispose: disposeFunc,
        loadHouse: loadHouseFunc
    }

    return editor
}
