/**
 * createEditor will create the Web Editor instance
 * @param {Number} width width of the viewport
 * @param {Number} height height of the viewport
 * @param {HTMLElement} dom parent DOM element to attach the WebGL canvas to
 */
declare const createEditor: (width: number, height: number, dom: HTMLElement) => {
    resize: (width: number, height: number) => void;
    dispose: () => void;
    loadHouse: (obj: string, mtl: string) => void;
};
export default createEditor;
