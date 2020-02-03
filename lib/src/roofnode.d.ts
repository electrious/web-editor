import { RoofPlate, RoofOperation } from './models/roofplate';
import { Object3D } from 'three';
import { Stream, Disposable } from '@most/types';
import { SceneTapEvent } from './sceneevent';
export interface RoofNode {
    roofId: string;
    roofUpdate: Stream<RoofOperation>;
    roofDelete: Stream<RoofOperation>;
    tapped: Stream<SceneTapEvent>;
    roofObject: Object3D;
    disposable: Disposable;
}
/**
 * create RoofNode for a RoofPlate
 * @param roof
 */
export declare function createRoofNode(roof: RoofPlate, isActive: Stream<boolean>): RoofNode;
