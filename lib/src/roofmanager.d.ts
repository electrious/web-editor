import { RoofPlate, RoofEdited } from './models/roofplate';
import { Object3D } from 'three';
import { Disposable, Stream } from '@most/types';
import { HouseMeshData } from './house';
export interface RoofManager {
    roofWrapper: Object3D;
    editedRoofs: Stream<RoofEdited[]>;
    disposable: Disposable;
}
/**
 * create RoofManager for an array of roofs
 * @param roofs
 */
export declare function createRoofManager(meshData: HouseMeshData, defRoofs: RoofPlate[]): RoofManager;
