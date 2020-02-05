import { Stream, Disposable } from '@most/types';
import { RoofPlate } from '../models/roofplate';
import { Object3D } from 'three';
import { SceneMouseMoveEvent } from '../sceneevent';
/**
 * RoofRecognizer wlll be able to let user add new roof
 */
export interface RoofRecognizer {
    marker: Object3D;
    addedNewRoof: Stream<RoofPlate>;
    disposable: Disposable;
}
/**
 * Create a roof recognizer object.
 * @param houseWrapper the house mesh wrapper object
 * @param roofs a stream of the current roof plates
 * @param mouseMove a stream of mouse move events in the scene
 * @returns RoofRecognizer
 */
export declare function createRoofRecognizer(houseWrapper: Object3D, roofs: Stream<RoofPlate[]>, mouseMove: Stream<SceneMouseMoveEvent>, canShow: Stream<boolean>): RoofRecognizer;
