import { InputEvents, TapEvent, DragEvent } from './input'
import { Camera, Object3D, Vector3, Raycaster, Vector2, Vec2 } from 'three'
import { Stream, Disposable, Scheduler } from '@most/types'
import { mkSink } from './sink'
import { snapshot, merge, tap } from '@most/core'

/**
 * SceneEvent is the event data sent to objects in the scene graph that's
 * been raycasted as the target
 */
export interface SceneEvent {
    distance: number
    point: Vector3
    event: TapEvent | DragEvent
}

function isTapEvent(event: TapEvent | DragEvent): event is TapEvent {
    return 'tapX' in event
}

/**
 * Any object that want to accept SceneEvent as target should implement this
 * interface
 */
export interface RaycastTarget {
    processEvent: (event: SceneEvent) => boolean
}

function toRaycastTarget(obj: any): RaycastTarget | null {
    return 'processEvent' in obj ? obj : null
}

interface EventWithSize {
    size: [number, number]
    event: TapEvent | DragEvent
}

function mkEventWithSize(
    size: [number, number],
    event: TapEvent | DragEvent
): EventWithSize {
    return {
        size: size,
        event: event
    }
}

function calcPosition(pos: Vector2, size: [number, number]): Vector2 {
    const w = size[0]
    const h = size[1]
    return new Vector2((pos.x / w) * 2 - 1, -(pos.y / h) * 2 + 1)
}

function getMousePosition(es: EventWithSize): Vector2 {
    const e = es.event
    return calcPosition(
        isTapEvent(e)
            ? new Vector2(e.tapX, e.tapY)
            : new Vector2(e.dragX, e.dragY),
        es.size
    )
}

export function setupRaycasting(
    camera: Camera,
    scene: Object3D,
    input: InputEvents,
    size: Stream<[number, number]>,
    scheduler: Scheduler
): Disposable {
    const raycaster = new Raycaster()

    const f = (es: EventWithSize) => {
        const tp = getMousePosition(es)
        raycaster.setFromCamera(tp, camera)
        const results = raycaster.intersectObject(scene, true)
        for (const k in results) {
            if (results.hasOwnProperty(k)) {
                const res = results[k]

                const rt = toRaycastTarget(res.object)
                if (rt != null) {
                    const canTestNext = rt.processEvent({
                        distance: res.distance,
                        point: res.point,
                        event: es.event
                    })

                    if (!canTestNext) {
                        break
                    }
                }
            }
        }
    }

    const sink = mkSink(f)
    const evts = merge(input.tapped, input.dragged)
    return snapshot(mkEventWithSize, size, evts).run(sink, scheduler)
}
