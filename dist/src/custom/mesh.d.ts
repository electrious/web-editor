import { Mesh, Geometry, Material, Vector3 } from 'three';
import { Tappable, SceneTapEvent, Draggable, SceneDragEvent } from '../sceneevent';
import { Stream } from '@most/types';
/**
 * Extend Mesh with Tappable and provide the tapEvents stream
 */
export declare class TappableMesh extends Mesh implements Tappable {
    tapEvents: Stream<SceneTapEvent>;
    private insertTap;
    constructor(geo: Geometry, mat: Material);
    tapped(event: SceneTapEvent): void;
}
/**
 * process the drag events in the stream to make sure all drag start with
 * dragStart and end with dragEnd.
 * @param evt
 */
export declare function validateDrag(evt: Stream<SceneDragEvent>): Stream<SceneDragEvent>;
/**
 * Calculate local delta distances for all drag events
 * @param evt
 * @param toLocal
 */
export declare function calcDragDelta(evt: Stream<SceneDragEvent>, toLocal: (v: Vector3) => Vector3 | null): Stream<Vector3>;
/**
 * Extend Mesh with Draggable and provide the dragEvents stream
 */
export declare class DraggableMesh extends Mesh implements Draggable {
    dragEvents: Stream<SceneDragEvent>;
    dragDelta: Stream<Vector3>;
    private insertDrag;
    constructor(geo: Geometry, mat: Material);
    dragged(event: SceneDragEvent): void;
}
/**
 * Extend Mesh with both Tappable and Draggable and provide the event streams
 */
export declare class TapDragMesh extends DraggableMesh implements Tappable {
    tapEvents: Stream<SceneTapEvent>;
    private insertTap;
    constructor(geo: Geometry, mat: Material);
    tapped(event: SceneTapEvent): void;
}
