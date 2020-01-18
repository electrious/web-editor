import { Vector3, Vector2, Object3D, Geometry, Material } from 'three';
import { Stream, Disposable } from '@most/types';
/**
 * defines all data for a Draggable Object
 */
export interface DraggableObject {
    object: Object3D;
    tapped: Stream<number>;
    position: Stream<Vector3>;
    isDragging: Stream<boolean>;
    disposable: Disposable;
}
/**
 * create a draggable object.
 * @param active a stream signalling if the current roof is active or not
 * @param position 2D start position of the object
 * @param customGeo optional custom geometry for the object
 * @param customMat optional custom material for the object
 * @returns DraggableObject
 */
export declare const createDraggableObject: (active: Stream<boolean>, index: number, position: Vector2, customGeo?: Geometry, customMat?: Material) => DraggableObject;
