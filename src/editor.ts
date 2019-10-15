import {
    Scene,
    PerspectiveCamera,
    WebGLRenderer,
    AmbientLight,
    BoxGeometry,
    MeshBasicMaterial,
    Mesh
} from 'three'
import { loadHouse } from './house'

export interface WebEditor {
    resize: (width: number, height: number) => void
    dispose: () => void
    loadHouse: (obj: string, mtl: string) => void
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
    camera.position.set(0, -5, 5)
    camera.lookAt(0, 0, 0)

    // add ambient light
    const light = new AmbientLight(0x404040)
    scene.add(light)

    const geometry = new BoxGeometry(1, 1, 1)
    const material = new MeshBasicMaterial({ color: 0x00ff00 })
    const cube = new Mesh(geometry, material)
    scene.add(cube)

    /**
     * function to update renderring of the WebGL scene
     */
    const renderFunc = () => {
        renderer.render(scene, camera)
    }

    const animate = () => {
        requestAnimationFrame(animate)
        cube.rotation.x += 0.01
        cube.rotation.y += 0.01
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

    const loadHouseFunc = (obj: string, mtl: string) => {
        loadHouse(obj, mtl, '').subscribe(ob => {
            scene.add(ob)
        })
    }

    const editor = {
        resize: resizeFunc,
        dispose: disposeFunc,
        loadHouse: loadHouseFunc
    }

    return editor
}
