import { Object3D, Vector2, Vector3 } from 'three'
import { Stream, Disposable } from '@most/types'
import { createAdapter } from '@most/adapter'
import {
    combine,
    multicast,
    map,
    combineArray,
    skip,
    loop,
    switchLatest,
    mergeArray,
    snapshot,
    merge
} from '@most/core'
import { createDraggableObject, DraggableObject } from '../ui/draggableobject'
import { mkSink } from '../sink'
import { defScheduler } from '../helper'
import { disposeBoth, dispose, disposeAll } from '@most/disposable'
import pluck from 'ramda/es/pluck'
import remove from 'ramda/es/remove'

const toVec2 = (v: Vector3): Vector2 => {
    return new Vector2(v.x, v.y)
}
const mkArray = (...args: Vector3[]) => {
    return args.map(toVec2)
}

export interface RoofEditor {
    roofVertices: Stream<Vector2[]>
    disposable: Disposable
}

// create roof corner markers
export const createRoofEditor = (
    parent: Object3D,
    active: Stream<boolean>,
    ps: Vector2[]
): RoofEditor => {
    // internal stream for currently active marker
    const [setActive, activeMarker] = createAdapter<number | null>()

    // internal stream for maintaning the roof active status
    const [setRoofActive, roofActive] = createAdapter<boolean>()

    const scheduler = defScheduler()

    // pipe the 'active' param event into internal roofActive stream
    const disposable1 = active.run(mkSink(setRoofActive), scheduler)

    // streams for new list of vertices
    const [updateVertList, vertices] = createAdapter<Vector2[]>()

    // function to create new markers for a list of vertices
    const mkMarkers = (ps: Vector2[]): DraggableObject[] => {
        const res = ps.map((pos: Vector2, idx: number) => {
            // determine whether the current marker should be active or not
            const f = (roofActive: boolean, activeIdx: number | null) => {
                return roofActive && (activeIdx == null || activeIdx == idx)
            }
            const isActive = multicast(combine(f, roofActive, activeMarker))

            // create the marker
            const marker = createDraggableObject(isActive, idx, pos)

            // if the current marker is being dragged, then it's active
            const g = (dragging: boolean): number | null => {
                return dragging ? idx : null
            }
            const d = map(g, marker.isDragging).run(
                mkSink(setActive),
                scheduler
            )

            marker.disposable = disposeBoth(marker.disposable, d)

            return marker
        })
        return res
    }

    // delete old marker objects and add new ones
    const attachObjs = (
        objs: DraggableObject[],
        newObjs: DraggableObject[]
    ) => {
        objs.forEach(o => {
            parent.remove(o.object)
            dispose(o.disposable)
        })

        newObjs.forEach(o => {
            parent.add(o.object)
        })

        setActive(null)

        return { seed: newObjs, value: newObjs }
    }

    // create new markers and attach them to the parent object.
    const markers = multicast(loop(attachObjs, [], map(mkMarkers, vertices)))

    // get new positions after dragging
    const getPosition = (o: DraggableObject[]) => {
        return skip(1, combineArray(mkArray, pluck('position', o)))
    }
    const vertsAfterDrag = switchLatest(map(getPosition, markers))

    // merge new vertices after dragging and vertices after deleting
    const newVertices = multicast(merge(vertices, vertsAfterDrag))

    // get delete event of tapping on a marker
    const getDelEvt = (o: DraggableObject[]) => {
        return mergeArray(pluck('tapped', o))
    }
    const delEvts = switchLatest(map(getDelEvt, markers))

    // calculate new vertices after deleting a vertex
    const delMarker = (ps: Vector2[], idx: number) => {
        return remove(idx, 1, ps)
    }
    const vertsAfterDel = snapshot(delMarker, newVertices, delEvts)

    // update the real vertex list after deleting
    const disposable2 = vertsAfterDel.run(
        mkSink(vs => {
            updateVertList(vs)
            setRoofActive(true)
        }),
        scheduler
    )

    // set the default vertices
    updateVertList(ps)

    return {
        // skip the first occurrence as it's the same values as provided
        roofVertices: newVertices,
        disposable: disposeAll([disposable1, disposable2])
    }
}
