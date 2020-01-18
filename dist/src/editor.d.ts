import { RoofPlate, RoofOperation } from './models/roofplate';
export declare type Size = [number, number];
/**
 * Public Interface for the main WebEditor
 */
export interface WebEditor {
    resize: (size: Size) => void;
    dispose: () => void;
    loadHouse: (leadId: number, roofs: RoofPlate[], roofUpdated: (r: RoofOperation) => void) => void;
}
/**
 * createEditor will create the Web Editor instance
 * @param {Number} width width of the viewport
 * @param {Number} height height of the viewport
 * @param {HTMLElement} dom parent DOM element to attach the WebGL canvas to
 */
declare function createEditor(width: number, height: number, elem: Element): WebEditor;
declare const _default: {
    createEditor: typeof createEditor;
};
export default _default;
