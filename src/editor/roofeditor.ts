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
    merge,
    constant
} from '@most/core'
import { createDraggableObject, DraggableObject } from '../ui/draggableobject'
import { mkSink } from '../sink'
import { defScheduler } from '../helper'
import { dispose, disposeAll } from '@most/disposable'
import pluck from 'ramda/es/pluck'
import remove from 'ramda/es/remove'
import curry from 'ramda/es/curry'

const toVec2 = (v: Vector3): Vector2 => {
    return new Vector2(v.x, v.y)
}
const mkArray = (...args: Vector3[]) => {
    return args.map(toVec2)
}

/**
 * create red markers for vertices
 */
const mkRedMarkers = curry(
    (
        roofActive: Stream<boolean>,
        activeMarker: Stream<number | null>,
        ps: Vector2[]
    ): DraggableObject[] => {
        const res = ps.map((pos: Vector2, idx: number) => {
            // determine whether the current marker should be active or not
            const f = (roofActive: boolean, activeIdx: number | null) => {
                return roofActive && (activeIdx == null || activeIdx == idx)
            }
            const isActive = multicast(combine(f, roofActive, activeMarker))

            // create the marker
            return createDraggableObject(isActive, idx, pos)
        })
        return res
    }
)

/**
 * get red markers' active status event stream
 * @param ms a stream of all red markers
 */
const getRedMarkerActiveStatus = (
    ms: Stream<DraggableObject[]>
): Stream<number | null> => {
    // get active marker when any one is being dragged
    const g = (m: DraggableObject, idx: number) => {
        return map(d => {
            return d ? idx : null
        }, m.isDragging)
    }

    const h = (ms: DraggableObject[]) => {
        return mergeArray(ms.map(g))
    }
    const statusForDragging = switchLatest(map(h, ms))

    // set active marker to null when all markers are just created
    const statusForNewMarker = constant(null, ms)

    return merge(statusForDragging, statusForNewMarker)
}

// delete old marker objects and add new ones. this will return a function
// that can be used in 'loop' directly
const attachObjs = (parent: Object3D) => {
    return (objs: DraggableObject[], newObjs: DraggableObject[]) => {
        objs.forEach(o => {
            parent.remove(o.object)
            dispose(o.disposable)
        })

        newObjs.forEach(o => {
            parent.add(o.object)
        })

        return { seed: newObjs, value: newObjs }
    }
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
    const scheduler = defScheduler()

    // internal stream for maintaning the roof active status
    const [setRoofActive, roofActive] = createAdapter<boolean>()

    // pipe the 'active' param event into internal roofActive stream
    const disposable1 = active.run(mkSink(setRoofActive), scheduler)

    // stream for new list of vertices
    const [updateVertList, vertices] = createAdapter<Vector2[]>()

    // internal stream for currently active marker
    const [setActive, activeMarker] = createAdapter<number | null>()

    // create new markers and attach them to the parent object.
    const markerObjs = map(mkRedMarkers(roofActive, activeMarker), vertices)
    const markers = multicast(loop(attachObjs(parent), [], markerObjs))

    // stream for active red marker
    const actMarker = getRedMarkerActiveStatus(markers)
    const disposable3 = actMarker.run(mkSink(setActive), scheduler)

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

            // reset rofoActive to true so that the 'combine' call in
            // mkMarkers func will be able to continue without waiting for
            // explicit event from the 'active' param stream
            setRoofActive(true)
        }),
        scheduler
    )

    // set the default vertices
    updateVertList(ps)

    return {
        // skip the first occurrence as it's the same values as provided
        roofVertices: newVertices,
        disposable: disposeAll([disposable1, disposable2, disposable3])
    }
}
