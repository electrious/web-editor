import {Scene, PerspectiveCamera, WebGLRenderer} from "three";

/**
 * createEditor will create the Web Editor instance
 * @param {Number} width width of the viewport
 * @param {Number} height height of the viewport
 * @param {HTMLElement} dom parent DOM element to attach the WebGL canvas to
 */
export const createEditor = (width, height, dom) => {
    let scene = new Scene();
    let camera = new PerspectiveCamera(75, width / height, 0.1, 1000);
    let renderer = new WebGLRenderer();

    renderer.setSize(width, height);

    // attach the WebGL canvas to a parent DOM element
    dom.appendChild(renderer.domElement)

    /**
     * function to update renderring of the WebGL scene
     */
    let renderFunc = () => {
        renderer.render(scene, camera);
    };

    /**
     * resize the editor viewport
     * @param {Number} width new width of the viewport
     * @param {Number} height new height of the viewport
     */ 
    let resizeFunc = (width, height) => {
        camera.aspect = width / height;
        renderer.setSize(width, height);
    }

    /**
     * dispose all resources used by the editor
     */
    let disposeFunc = () => {
        scene.dispose();
    }

    return {
        render: renderFunc,
        resize: resizeFunc,
        dispose: disposeFunc
    }
}
