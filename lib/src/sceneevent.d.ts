import { InputEvents, DragEvent, DragType } from './input';
import { Camera, Object3D, Vector3, Vector2, Face3 } from 'three';
import { Stream, Disposable } from '@most/types';
import { Size } from './editor';
/**
 * tap events sent to 3D objects
 */
export interface SceneTapEvent {
    distance: number;
    point: Vector3;
    domPosition: Vector2;
}
/**
 * mousemove events sent to 3D objects
 */
export interface SceneMouseMoveEvent {
    distance: number;
    point: Vector3;
    face: Face3;
    domPosition: Vector2;
}
/**
 * drag events sent to 3D objects
 */
export interface SceneDragEvent {
    type: DragType;
    distance: number;
    point: Vector3;
}
export declare function isDragStart(e: SceneDragEvent): boolean;
export declare function isDrag(e: SceneDragEvent): boolean;
export declare function isDragEnd(e: SceneDragEvent): boolean;
/**
 * add end event to SceneDragEvent stream if there's no input for a while and
 * no end event.
 * @param evt input event stream
 */
export declare function mkDragEndable(evt: Stream<SceneDragEvent>): Stream<SceneDragEvent>;
/**
 * objects that can process tap event
 */
export interface Tappable {
    tapped: (event: SceneTapEvent) => void;
}
/**
 * objects that can process mouseover event
 */
export interface MouseMovable {
    mouseMove: (event: SceneMouseMoveEvent) => void;
}
/**
 * objects that can process drag event
 */
export interface Draggable {
    dragged: (event: SceneDragEvent) => void;
}
/**
 * interface for the raycasting process setup result.
 */
export interface RaycastSetup {
    dragEvent: Stream<DragEvent>;
    disposable: Disposable;
}
/**
 * setup all raycasting needed to process user inputs and send them to the
 * corresponding 3D object in the scene
 * @param camera threejs camera
 * @param scene threejs scene graph
 * @param input user input event streams
 * @param size stream of the current threejs viewport size
 * @param scheduler used for all stream
 */
export declare function setupRaycasting(camera: Camera, scene: Object3D, input: InputEvents, size: Stream<Size>): RaycastSetup;
