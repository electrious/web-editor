import { InputEvents, TapEvent, DragEvent, DragType } from './input'
import {
    Camera,
    Object3D,
    Vector3,
    Raycaster,
    Vector2,
    Intersection
} from 'three'
import { Stream, Disposable } from '@most/types'
import { mkSink } from './sink'
import { snapshot, multicast, debounce, map } from '@most/core'
import { Size } from './editor'
import { disposeBoth } from '@most/disposable'
import { defScheduler, unwrap } from './helper'
import merge from 'ramda/es/merge'

/**
 * tap events sent to 3D objects
 */
export interface SceneTapEvent {
    distance: number
    point: Vector3
    domPosition: Vector2 // original mouse/touch event position
}

/**
 * drag events sent to 3D objects
 */
export interface SceneDragEvent {
    type: DragType
    distance: number
    point: Vector3
}

export function isDragStart(e: SceneDragEvent): boolean {
    return e.type == DragType.DragStart
}

export function isDrag(e: SceneDragEvent): boolean {
    return e.type == DragType.Drag
}

export function isDragEnd(e: SceneDragEvent): boolean {
    return e.type == DragType.DragEnd
}

/**
 * add end event to SceneDragEvent stream if there's no input for a while and
 * no end event.
 * @param evt input event stream
 */
export function mkDragEndable(
    evt: Stream<SceneDragEvent>
): Stream<SceneDragEvent> {
    // wait for 2 seconds and see if there're new events
    // if not, make sure the last one is DragEnd
    const e = debounce(2000, evt)

    const f = (e: SceneDragEvent): SceneDragEvent | null => {
        if (e.type != DragType.DragEnd) {
            return {
                type: DragType.DragEnd,
                distance: e.distance,
                point: e.point
            }
        }

        return null
    }

    return merge(evt, unwrap(map(f, e)))
}

/**
 * objects that can process tap event
 */
export interface Tappable {
    tapped: (event: SceneTapEvent) => void
}

function toTappable(obj: any): Tappable | null {
    return 'tapped' in obj ? obj : null
}
/**
 * objects that can process drag event
 */
export interface Draggable {
    dragged: (event: SceneDragEvent) => void
}

function toDraggable(obj: any): Draggable | null {
    return 'dragged' in obj ? obj : null
}

/**
 * convert mouse/touch position to values between -1 and 1
 * @param pos
 * @param size
 */
function calcPosition(pos: Vector2, size: Size): Vector2 {
    const w = size[0]
    const h = size[1]
    return new Vector2((pos.x / w) * 2 - 1, -(pos.y / h) * 2 + 1)
}

/**
 * convert a TapEvent to position used for raycasting
 * @param size
 * @param e
 */
function tapPosition(size: Size, e: TapEvent): Vector2 {
    return calcPosition(new Vector2(e.tapX, e.tapY), size)
}

/**
 * convert a DragEvent to position used for raycasting
 * @param size
 * @param e
 */
function dragPosition(size: Size, e: DragEvent): Vector2 {
    return calcPosition(new Vector2(e.dragX, e.dragY), size)
}

/**
 * Find first object in the intersections array that is Tappable and send the
 * event to it.
 * @param objs
 */
function processTapObjects(domPos: Vector2, objs: Intersection[]) {
    for (const key in objs) {
        if (objs.hasOwnProperty(key)) {
            const res = objs[key]

            const t = toTappable(res.object)
            if (t != null) {
                t.tapped({
                    distance: res.distance,
                    point: res.point,
                    domPosition: domPos
                })

                break
            }
        }
    }
}

/**
 * interface for the raycasting process setup result.
 */
export interface RaycastSetup {
    dragEvent: Stream<DragEvent>
    disposable: Disposable
}

/**
 * Find first object in the intersections array that is Draggable and send the
 * drag event to it
 * @param objs
 * @param e
 */
function processDragObjects(
    objs: Intersection[],
    e: DragEvent
): DragEvent | null {
    for (const key in objs) {
        if (objs.hasOwnProperty(key)) {
            const res = objs[key]
            const t = toDraggable(res.object)

            if (t != null) {
                t.dragged({
                    type: e.dragType,
                    distance: res.distance,
                    point: res.point
                })

                return null
            }
        }
    }

    return e
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
export function setupRaycasting(
    camera: Camera,
    scene: Object3D,
    input: InputEvents,
    size: Stream<Size>
): RaycastSetup {
    const raycaster = new Raycaster()

    // function to do real raycasting
    const doRaycast = (tp: Vector2): Intersection[] => {
        raycaster.setFromCamera(tp, camera)
        return raycaster.intersectObject(scene, true)
    }

    // raycast tap events
    const raycastTap = (s: Size, e: TapEvent) => {
        const domPos = new Vector2(e.tapX, e.tapY)
        const raycastRes = doRaycast(tapPosition(s, e))

        processTapObjects(domPos, raycastRes)
    }

    const scheduler = defScheduler()
    const sink = mkSink()
    const disposeTap = snapshot(raycastTap, size, input.tapped).run(
        sink,
        scheduler
    )

    // raycast drag events
    const raycastDrag = (s: Size, e: DragEvent): DragEvent | null => {
        const results = doRaycast(dragPosition(s, e))
        return processDragObjects(results, e)
    }

    const unraycastedDrag = snapshot(raycastDrag, size, input.dragged)
    const disposeDrag = unraycastedDrag.run(sink, scheduler)

    return {
        dragEvent: multicast(unwrap(unraycastedDrag)),
        disposable: disposeBoth(disposeTap, disposeDrag)
    }
}
