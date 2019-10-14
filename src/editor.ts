import {Scene, PerspectiveCamera, WebGLRenderer, Vector3, AmbientLight} from "three";
import { loadHouse } from "./house";

/**
 * createEditor will create the Web Editor instance
 * @param {Number} width width of the viewport
 * @param {Number} height height of the viewport
 * @param {HTMLElement} dom parent DOM element to attach the WebGL canvas to
 */
export const createEditor = (width: number, height: number, dom: HTMLElement) => {
    let scene = new Scene();
    let camera = new PerspectiveCamera(75, width / height, 0.1, 1000);
    let renderer = new WebGLRenderer();

    renderer.setSize(width, height);

    // attach the WebGL canvas to a parent DOM element
    dom.appendChild(renderer.domElement);

    // set the camera position and orient it toward the center
    camera.position = new Vector3(0, -1, 1);
    camera.lookAt(0, 0, 0);

    // add ambient light
    let light = new AmbientLight(0x404040);
    scene.add(light);

    /**
     * function to update renderring of the WebGL scene
     */
    let renderFunc = () => {
        renderer.render(scene, camera);
    };

    let animate = () => {
        requestAnimationFrame(animate);

        renderFunc();
    }
    // start the renderring
    animate();

    /**
     * resize the editor viewport
     * @param {Number} width new width of the viewport
     * @param {Number} height new height of the viewport
     */ 
    let resizeFunc = (width: number, height: number) => {
        camera.aspect = width / height;
        renderer.setSize(width, height);
    }

    /**
     * dispose all resources used by the editor
     */
    let disposeFunc = () => {
        scene.dispose();
    }

    let loadHouseFunc = (obj: string, mtl: string) => {
        loadHouse(obj, mtl, "").subscribe((ob) => {
            scene.add(ob);
        })
    }

    let editor = {
        resize: resizeFunc,
        dispose: disposeFunc,
        loadHouse: loadHouseFunc
    }

    return editor;
}
