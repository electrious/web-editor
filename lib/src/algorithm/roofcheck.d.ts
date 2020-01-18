import { Object3D } from 'three';
import { SceneMouseMoveEvent } from '../sceneevent';
import { RoofPlate } from '../models/roofplate';
/**
 * check if there can be a roof at the point under mouse
 * @param house
 * @param roofs
 * @param e
 */
export declare function couldBeRoof(house: Object3D, roofs: RoofPlate[], e: SceneMouseMoveEvent): boolean;
