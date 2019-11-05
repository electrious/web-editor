import { Object3D, Vector2, Vector3 } from 'three'
import { Stream, Disposable } from '@most/types'
import { createAdapter } from '@most/adapter'
import { combine, multicast, map, combineArray, skip } from '@most/core'
import { createDraggableObject } from '../ui/draggableobject'
import { mkSink } from '../sink'
import { defScheduler } from '../helper'
import { disposeBoth, disposeAll } from '@most/disposable'
import pluck from 'ramda/es/pluck'

export interface RoofEditor {
    roofVertices: Stream<Vector2[]>
    disposable: Disposable
}

// create roof corner markers
export const createRoofEditor = (
    obj: Object3D,
    roofActive: Stream<boolean>,
    ps: Vector2[]
): RoofEditor => {
    // internal stream for currently active marker
    const [setActive, activeMarker] = createAdapter<number | null>()

    const markers = ps.map((pos: Vector2, idx: number) => {
        // determine whether the current marker should be active or not
        const f = (roofActive: boolean, activeIdx: number | null): boolean => {
            return roofActive && (activeIdx == null || activeIdx == idx)
        }
        const isActive = multicast(combine(f, roofActive, activeMarker))

        // create the marker
        const marker = createDraggableObject(isActive, pos)

        // if the current marker is being dragged, then it's active
        const g = (dragging: boolean): number | null => {
            return dragging ? idx : null
        }
        const d = map(g, marker.isDragging).run(
            mkSink(setActive),
            defScheduler()
        )

        marker.disposable = disposeBoth(marker.disposable, d)

        return marker
    })

    // by default, all markers should be active for an active roof
    setActive(null)

    markers.forEach(m => {
        obj.add(m.object)
    })

    const toVec2 = (v: Vector3): Vector2 => {
        return new Vector2(v.x, v.y)
    }
    const mkArray = (...args: Vector3[]) => {
        return args.map(toVec2)
    }

    return {
        // skip the first occurrence as it's the same values as provided
        roofVertices: skip(
            1,
            combineArray(mkArray, pluck('position', markers))
        ),
        disposable: disposeAll(pluck('disposable', markers))
    }
}
