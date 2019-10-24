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
import { snapshot } from '@most/core'
import { Size } from './editor'
import { disposeBoth } from '@most/disposable'
import compose from 'ramda/es/compose'
import { defScheduler } from './helper'

/**
 * tap events sent to 3D objects
 */
export interface SceneTapEvent {
    distance: number
    point: Vector3
}

/**
 * drag events sent to 3D objects
 */
export interface SceneDragEvent {
    type: DragType
    distance: number
    point: Vector3
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
function processTapObjects(objs: Intersection[]) {
    for (const key in objs) {
        if (objs.hasOwnProperty(key)) {
            const res = objs[key]

            const t = toTappable(res.object)
            if (t != null) {
                t.tapped({
                    distance: res.distance,
                    point: res.point
                })

                break
            }
        }
    }
}

/**
 * Find first object in the intersections array that is Draggable and send the
 * drag event to it
 * @param objs
 * @param e
 */
function processDragObjects(objs: Intersection[], e: DragEvent) {
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

                break
            }
        }
    }
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
): Disposable {
    const raycaster = new Raycaster()

    // function to do real raycasting
    const doRaycast = (tp: Vector2): Intersection[] => {
        raycaster.setFromCamera(tp, camera)
        return raycaster.intersectObject(scene, true)
    }

    // raycast tap events
    const raycastTap = compose(
        processTapObjects,
        doRaycast
    )

    // raycast drag events
    const raycastDrag = (arg: [Vector2, DragEvent]) => {
        raycaster.setFromCamera(arg[0], camera)
        const results = raycaster.intersectObject(scene, true)
        processDragObjects(results, arg[1])
    }

    const scheduler = defScheduler()
    const disposeTap = snapshot(tapPosition, size, input.tapped).run(
        mkSink(raycastTap),
        scheduler
    )

    const f = (s: Size, e: DragEvent): [Vector2, DragEvent] => {
        return [dragPosition(s, e), e]
    }

    const disposeDrag = snapshot(f, size, input.dragged).run(
        mkSink(raycastDrag),
        scheduler
    )

    return disposeBoth(disposeTap, disposeDrag)
}
