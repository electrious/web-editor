import { JSRoofPlate, RoofEdited } from './models/roofplate';
export declare type Size = [number, number];
/**
 * Public Interface for the main WebEditor
 */
export interface WebEditor {
    resize: (width: number, height: number) => void;
    dispose: () => void;
    loadHouse: (dataServerUrl: string, leadId: number, roofs: JSRoofPlate[], roofEdited: (r: RoofEdited[]) => void) => void;
}
/**
 * createEditor will create the Web Editor instance
 * @param {Number} width width of the viewport
 * @param {Number} height height of the viewport
 * @param {HTMLElement} dom parent DOM element to attach the WebGL canvas to
 */
export declare function createEditor(width: number, height: number, elem: Element): WebEditor;
