import { Mesh, Geometry, Material, Vector3 } from 'three'
import {
    Tappable,
    SceneTapEvent,
    Draggable,
    SceneDragEvent
} from '../sceneevent'
import { Stream } from '@most/types'
import { createAdapter } from '@most/adapter'
import clone from 'ramda/es/clone'
import { map, loop } from '@most/core'
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
 * Calculate local delta distances for all drag events
 * @param evt
 * @param toLocal
 */
function calcDragDelta(
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
        this.dragEvents = s
        this.insertDrag = f

        this.dragDelta = calcDragDelta(s, v => {
            const obj = this.parent
            return obj == null ? null : obj.worldToLocal(v)
        })
    }

    dragged(event: SceneDragEvent) {
        this.insertDrag(event)
    }
}
