import { Stream, Disposable } from '@most/types';
export interface TapEvent {
    tapX: number;
    tapY: number;
}
/**
 * MouseMoveEvent encode the mouse position for MouseMove event.
 */
export interface MouseMoveEvent {
    mouseX: number;
    mouseY: number;
}
export declare enum DragType {
    DragStart = 0,
    Drag = 1,
    DragEnd = 2
}
export interface DragEvent {
    dragType: DragType;
    dragX: number;
    dragY: number;
    deltaX: number;
    deltaY: number;
}
export interface InputEvents {
    tapped: Stream<TapEvent>;
    zoomed: Stream<WheelEvent>;
    dragged: Stream<DragEvent>;
    shiftDragged: Stream<DragEvent>;
    mouseMove: Stream<MouseMoveEvent>;
    disposable: Disposable;
}
/**
 * Setup the input system for an element. It will return the InputEvents object
 * with all supported event streams.
 * @param elem
 */
export declare function setupInput(elem: Element): InputEvents;
