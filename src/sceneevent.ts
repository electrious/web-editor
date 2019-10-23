import { InputEvents, TapEvent, DragEvent, DragType } from './input'
import {
    Camera,
    Object3D,
    Vector3,
    Raycaster,
    Vector2,
    Intersection
} from 'three'
import { Stream, Disposable, Scheduler } from '@most/types'
import { mkSink } from './sink'
import { snapshot } from '@most/core'
import { Size } from './editor'
import { disposeBoth } from '@most/disposable'

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
    tapped: (event: SceneTapEvent) => boolean
}

function toTappable(obj: any): Tappable | null {
    return 'tapped' in obj ? obj : null
}
/**
 * objects that can process drag event
 */
export interface Draggable {
    dragged: (event: SceneDragEvent) => boolean
}

function toDraggable(obj: any): Draggable | null {
    return 'dragged' in obj ? obj : null
}

function calcPosition(pos: Vector2, size: Size): Vector2 {
    const w = size[0]
    const h = size[1]
    return new Vector2((pos.x / w) * 2 - 1, -(pos.y / h) * 2 + 1)
}

function tapPosition(size: Size, e: TapEvent): Vector2 {
    return calcPosition(new Vector2(e.tapX, e.tapY), size)
}

function dragPosition(size: Size, e: DragEvent): Vector2 {
    return calcPosition(new Vector2(e.dragX, e.dragY), size)
}

function processTapObjects(objs: Intersection[]) {
    for (const key in objs) {
        if (objs.hasOwnProperty(key)) {
            const res = objs[key]

            const t = toTappable(res.object)
            if (t != null) {
                const canTestNext = t.tapped({
                    distance: res.distance,
                    point: res.point
                })

                if (!canTestNext) break
            }
        }
    }
}

function processDragObjects(objs: Intersection[], e: DragEvent) {
    for (const key in objs) {
        if (objs.hasOwnProperty(key)) {
            const res = objs[key]
            const t = toDraggable(res.object)

            if (t != null) {
                const canTestNext = t.dragged({
                    type: e.dragType,
                    distance: res.distance,
                    point: res.point
                })

                if (!canTestNext) break
            }
        }
    }
}

export function setupRaycasting(
    camera: Camera,
    scene: Object3D,
    input: InputEvents,
    size: Stream<Size>,
    scheduler: Scheduler
): Disposable {
    const raycaster = new Raycaster()

    const raycastTap = (tp: Vector2) => {
        raycaster.setFromCamera(tp, camera)
        const results = raycaster.intersectObject(scene, true)
        processTapObjects(results)
    }

    const raycastDrag = (arg: [Vector2, DragEvent]) => {
        raycaster.setFromCamera(arg[0], camera)
        const results = raycaster.intersectObject(scene, true)
        processDragObjects(results, arg[1])
    }

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
