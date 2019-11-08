import { Mesh, Geometry, Material, Vector3 } from 'three'
import {
    Tappable,
    SceneTapEvent,
    Draggable,
    SceneDragEvent,
    isDragStart,
    isDrag,
    isDragEnd,
    mkDragEndable
} from '../sceneevent'
import { Stream } from '@most/types'
import { createAdapter } from '@most/adapter'
import clone from 'ramda/es/clone'
import { map, loop, multicast } from '@most/core'
import { unwrap } from '../helper'
import { DragType } from '../input'

/**
 * Extend Mesh with Tappable and provide the tapEvents stream
 */
export class TappableMesh extends Mesh implements Tappable {
    // tap event streams
    tapEvents: Stream<SceneTapEvent>

    // private function to insert new tap events to the stream
    private insertTap: (event: SceneTapEvent) => void

    constructor(geo: Geometry, mat: Material) {
        super(geo, mat)

        const [f, s] = createAdapter()
        this.tapEvents = s
        this.insertTap = f
    }

    tapped(event: SceneTapEvent) {
        this.insertTap(event)
    }
}

/**
 * process the drag events in the stream to make sure all drag start with
 * dragStart and end with dragEnd.
 * @param evt
 */
export function validateDrag(
    evt: Stream<SceneDragEvent>
): Stream<SceneDragEvent> {
    const f = (
        canDrag: boolean,
        e: SceneDragEvent
    ): { seed: boolean; value: SceneDragEvent | null } => {
        if (isDragStart(e)) {
            if (canDrag) {
                // if there's a repeated dragStart, omit it.
                return { seed: true, value: null }
            }
            return { seed: true, value: e }
        } else if (canDrag && isDrag(e)) {
            // only send drag event downstream if there was a drag start event
            return { seed: true, value: e }
        } else if (isDragEnd(e)) {
            // if the drag already ended, then omit the new End event.
            if (!canDrag) {
                return { seed: false, value: null }
            }
            // otherwise, stop the dragging, and send the end event
            return { seed: false, value: e }
        } else {
            // unknown state, omit the event
            return { seed: false, value: null }
        }
    }

    return unwrap(loop(f, false, evt))
}

/**
 * Calculate local delta distances for all drag events
 * @param evt
 * @param toLocal
 */
export function calcDragDelta(
    evt: Stream<SceneDragEvent>,
    toLocal: (v: Vector3) => Vector3 | null
): Stream<Vector3> {
    // default drag event as seed for loop
    const def: SceneDragEvent = {
        type: DragType.DragStart,
        distance: 0,
        point: new Vector3(0, 0, 0)
    }

    // convert drag event to use local coordinate system
    const e = unwrap(
        map(d => {
            const p = toLocal(d.point)
            if (p != null) {
                const nd = clone(d)
                nd.point = p
                return nd
            }
            return null
        }, evt)
    )

    // calculate delta between drag events
    const delta = (
        oe: SceneDragEvent,
        e: SceneDragEvent
    ): { seed: SceneDragEvent; value: Vector3 } => {
        if (e.type == DragType.DragStart) {
            return { seed: e, value: new Vector3(0, 0, 0) }
        } else {
            const delta = clone(e.point)
            delta.addScaledVector(oe.point, -1)

            return { seed: e, value: delta }
        }
    }

    return loop(delta, def, e)
}

/**
 * Extend Mesh with Draggable and provide the dragEvents stream
 */
export class DraggableMesh extends Mesh implements Draggable {
    // drag event streams
    dragEvents: Stream<SceneDragEvent>

    // drag delta in parent's coordinate system
    dragDelta: Stream<Vector3>

    // private function to insert drag events into the stream
    private insertDrag: (event: SceneDragEvent) => void

    constructor(geo: Geometry, mat: Material) {
        super(geo, mat)

        const [f, s] = createAdapter()
        this.dragEvents = multicast(s)
        this.insertDrag = f

        this.dragDelta = multicast(
            calcDragDelta(this.dragEvents, v => {
                const obj = this.parent
                return obj == null ? null : obj.worldToLocal(v)
            })
        )
    }

    dragged(event: SceneDragEvent) {
        this.insertDrag(event)
    }
}

/**
 * Extend Mesh with both Tappable and Draggable and provide the event streams
 */
export class TapDragMesh extends DraggableMesh implements Tappable {
    // tap event streams
    tapEvents: Stream<SceneTapEvent>

    // private function to insert new tap events to the stream
    private insertTap: (event: SceneTapEvent) => void

    constructor(geo: Geometry, mat: Material) {
        super(geo, mat)

        const [tf, ts] = createAdapter()
        this.tapEvents = ts
        this.insertTap = tf
    }

    tapped(event: SceneTapEvent) {
        this.insertTap(event)
    }
}
