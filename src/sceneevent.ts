import { InputEvents, TapEvent, DragEvent } from './input'
import { Camera, Object3D, Vector3, Raycaster, Vector2 } from 'three'
import { Disposable, Scheduler } from '@most/types'
import { mkSink } from './sink'
import { disposeAll } from '@most/disposable'

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

export function setupRaycasting(
    camera: Camera,
    scene: Object3D,
    input: InputEvents,
    scheduler: Scheduler
): Disposable {
    const raycaster = new Raycaster()

    const f = (e: TapEvent | DragEvent) => {
        const tp = isTapEvent(e)
            ? new Vector2(e.tapX, e.tapY)
            : new Vector2(e.dragX, e.dragY)

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
                        event: e
                    })

                    if (!canTestNext) {
                        break
                    }
                }
            }
        }
    }

    const sink = mkSink(f)
    const dispose1 = input.tapped.run(sink, scheduler)
    const dispose2 = input.dragged.run(sink, scheduler)

    return disposeAll([dispose1, dispose2])
}
