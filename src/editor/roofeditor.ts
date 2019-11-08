import {
    Object3D,
    Vector2,
    Vector3,
    MeshBasicMaterial,
    CircleGeometry
} from 'three'
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
import memoizeWith from 'ramda/es/memoizeWith'
import always from 'ramda/es/always'
import { TappableMesh } from '../custom/mesh'
import zipWith from 'ramda/es/zipWith'
import tail from 'ramda/es/tail'
import take from 'ramda/es/take'
import takeLast from 'ramda/es/takeLast'
import concat from 'ramda/es/concat'
import filter from 'ramda/es/filter'
import insert from 'ramda/es/insert'
import append from 'ramda/es/append'
import head from 'ramda/es/head'

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

/**
 * create material for the green marker. this function is memoized so the material
 * is created once and shared.
 */
const greenMaterial = memoizeWith(always('green_material'), () => {
    return new MeshBasicMaterial({ color: 0x22ff22 })
})

/**
 * create a green marker object
 */
const mkGreenMarker = (p: Vector2): TappableMesh => {
    const geo = new CircleGeometry(0.3, 32)
    const mat = greenMaterial()
    const m = new TappableMesh(geo, mat)
    m.position.set(p.x, p.y, 0.01)

    return m
}

/**
 * internal object for green marker point data
 */
interface GreenMarkerPoint {
    position: Vector2
    vertIndex: number // the index to be used to insert new vertex
}

interface GreenMarker {
    mesh: TappableMesh
    point: GreenMarkerPoint
}

/**
 * given a list of vertices position, calculate all middle points
 * @param vertices
 */
const greenMarkerPositions = (vertices: Vector2[]): GreenMarkerPoint[] => {
    if (vertices.length <= 1) {
        return []
    }

    // calculate the distance and marker point for two point
    const f = (v: [Vector2, number], v2: Vector2) => {
        const v1 = v[0]
        const idx = v[1]
        const markerPoint: GreenMarkerPoint = {
            position: new Vector2((v1.x + v2.x) / 2, (v1.y + v2.y) / 2),
            vertIndex: idx + 1
        }
        return { dist: v1.distanceTo(v2), point: markerPoint }
    }

    // take all vertices and their index
    const v1List = vertices.map((v, i): [Vector2, number] => [v, i])
    // make a new list with the head of origin list put to end
    const v2List = append(head(vertices), tail(vertices))
    // calculate the distance and points between vertex pairs
    const d = zipWith(f, v1List, v2List)

    // filter function
    const g = (d: { dist: number }) => {
        return d.dist > 1
    }

    // extract the marker point
    const h = (d: { point: GreenMarkerPoint }) => {
        return d.point
    }

    return filter(g, d).map(h)
}

const mkGreenMarkers = (
    parent: Object3D,
    active: Stream<boolean>,
    vertices: Stream<Vector2[]>
): [Stream<GreenMarkerPoint>, Disposable] => {
    const mPosLst = map(greenMarkerPositions, vertices)

    const updatePos = (o: GreenMarker, p: GreenMarkerPoint) => {
        o.mesh.position.set(p.position.x, p.position.y, 0.01)
        o.point = p
        return o
    }
    // function to create/delete/update green marker objects based on new list
    // of GreenMarkerPoint
    const f = (
        oldObjs: GreenMarker[],
        ps: GreenMarkerPoint[]
    ): { seed: GreenMarker[]; value: GreenMarker[] } => {
        let res: GreenMarker[] = []
        if (oldObjs.length == ps.length) {
            res = zipWith(updatePos, oldObjs, ps)
        } else if (oldObjs.length < ps.length) {
            const updObjs = zipWith(
                updatePos,
                oldObjs,
                take(oldObjs.length, ps)
            )
            const newObjs = takeLast(ps.length - oldObjs.length, ps).map(p => {
                const m = mkGreenMarker(p.position)
                return { mesh: m, point: p }
            })
            newObjs.forEach(o => {
                parent.add(o.mesh)
            })

            res = concat(updObjs, newObjs)
        } else if (oldObjs.length > ps.length) {
            const updObjs = zipWith(updatePos, take(ps.length, oldObjs), ps)
            const delObjs = takeLast(oldObjs.length - ps.length, oldObjs)
            delObjs.forEach(o => {
                parent.remove(o.mesh)
            })

            res = updObjs
        }

        return { seed: res, value: res }
    }

    const markers = multicast(loop(f, [], mPosLst))

    // set active status for all markers
    const setActive = (ms: GreenMarker[], active: boolean) => {
        ms.forEach(m => {
            m.mesh.visible = active
        })
    }

    const d = combine(setActive, markers, active).run(mkSink(), defScheduler())

    // transform tap event to add vertex event
    const getTap = (m: GreenMarker) => {
        return constant(m.point, m.mesh.tapEvents)
    }

    const getTapForAll = (ms: GreenMarker[]) => {
        return mergeArray(ms.map(getTap))
    }

    const tapEvts = switchLatest(map(getTapForAll, markers))

    return [tapEvts, d]
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

    // merge new vertices after dragging and vertices after adding/deleting
    const newVertices = multicast(merge(vertices, vertsAfterDrag))

    const greenActive = multicast(
        combine(
            (ra: boolean, am: number | null) => {
                return ra && am == null
            },
            roofActive,
            activeMarker
        )
    )
    // create green markers for adding new vertices
    const [toAddVert, disposable4] = mkGreenMarkers(
        parent,
        greenActive,
        newVertices
    )
    const addVert = (ps: Vector2[], p: GreenMarkerPoint) => {
        return insert(p.vertIndex, p.position, ps)
    }
    const vertsAfterAdd = snapshot(addVert, newVertices, toAddVert)

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

    // update the real vertex list after adding/deleting
    const disposable2 = merge(vertsAfterAdd, vertsAfterDel).run(
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

    // set roof to be inactive by default
    setRoofActive(false)

    return {
        // skip the first occurrence as it's the same values as provided
        roofVertices: newVertices,
        disposable: disposeAll([
            disposable1,
            disposable2,
            disposable3,
            disposable4
        ])
    }
}
