import { RoofPlate } from './models/roofplate'
import {
    Object3D,
    Vector2,
    Shape,
    ShapeGeometry,
    MeshBasicMaterial,
    Vector3
} from 'three'
import { TappableMesh } from './custom/mesh'
import memoizeWith from 'ramda/es/memoizeWith'
import always from 'ramda/es/always'
import { Stream, Disposable } from '@most/types'
import { SceneTapEvent } from './sceneevent'
import { mkSink } from './sink'
import { defScheduler } from './helper'
import { createDraggableObject } from './ui/draggableobject'
import pluck from 'ramda/es/pluck'
import { disposeAll, dispose, disposeBoth } from '@most/disposable'
import {
    combineArray,
    skip,
    map,
    multicast,
    debounce,
    combine
} from '@most/core'
import { createAdapter } from '@most/adapter'
import curry from 'ramda/es/curry'
import compose from 'ramda/es/compose'
import fmap from 'ramda/es/map'

export interface RoofNode {
    roof: Stream<RoofPlate>
    tapped: Stream<SceneTapEvent>
    roofObject: Object3D
    disposable: Disposable
}

/**
 * Get the default material for roofplate. This function is memoized so the
 * actual material is created only once and shared.
 */
const getDefMaterial = memoizeWith(always('def_material'), () => {
    const mat = new MeshBasicMaterial({ color: 0xffffbb })
    mat.transparent = true
    mat.opacity = 0.7

    return mat
})

/**
 * Get the material for an active roofpalte. This function is memoized so the
 * actual material is created only once and shared.
 */
const getActiveMaterial = memoizeWith(always('active_material'), () => {
    const mat = new MeshBasicMaterial({ color: 0xffff88 })
    mat.transparent = true
    mat.opacity = 0.9

    return mat
})

// create roof mesh
function createRoofMesh(
    ps: Vector2[],
    active?: boolean | undefined
): TappableMesh {
    // create a ShapeGeometry with the Shape from border points
    const shp = new Shape(ps)

    const mat = active ? getActiveMaterial() : getDefMaterial()
    return new TappableMesh(new ShapeGeometry(shp), mat)
}

// create roof corner markers
const createVertexMarkers = (
    obj: Object3D,
    active: Stream<boolean>,
    ps: Vector2[]
): [Stream<Vector2[]>, Disposable] => {
    // internal stream for currently active marker
    const [setActive, activeMarker] = createAdapter<number | null>()

    const markers = ps.map((pos: Vector2, idx: number) => {
        // determine whether the current marker should be active or not
        const f = (roofActive: boolean, activeIdx: number | null): boolean => {
            return roofActive && (activeIdx == null || activeIdx == idx)
        }
        const isActive = multicast(combine(f, active, activeMarker))

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

    return [
        // skip the first occurrence as it's the same values as provided
        skip(1, combineArray(mkArray, pluck('position', markers))),
        disposeAll(pluck('disposable', markers))
    ]
}

const updateRoofPlate = curry((roof: RoofPlate, ps: Vector3[]) => {
    roof.borderPoints = ps
    return roof
})

/**
 * create RoofNode for a RoofPlate
 * @param roof
 */
export function createRoofNode(
    roof: RoofPlate,
    isActive: Stream<boolean>
): RoofNode {
    const obj = new Object3D()
    obj.name = 'roofplate'

    // set the roof node position
    const c = roof.center
    obj.position.set(c.x, c.y, c.z + 0.05)

    // rotate the roof node to the right azimuth and slope angles
    obj.rotateZ(-roof.azimuth.rad)
    obj.rotateX(-roof.slope.rad)

    // make sure the matrix and matrixWorld are updated immediately after
    // position and rotation changed, so that worldToLocal can use them
    // to convert coordinates correctly
    obj.updateMatrix()
    obj.updateMatrixWorld()

    // convert all roof border points to local coordinate
    // and get only the x,y coordinates
    const ps = roof.borderPoints.map(p => {
        const np = obj.worldToLocal(p)
        return new Vector2(np.x, np.y)
    })

    const scheduler = defScheduler()

    // add the roof mesh
    let mesh = createRoofMesh(ps)
    obj.add(mesh)

    // update mesh material when the roof is activated/deactivated
    const disposable1 = isActive.run(
        mkSink((active: boolean) => {
            mesh.material = active ? getActiveMaterial() : getDefMaterial()
        }),
        scheduler
    )

    // create the vertex markers
    const [newPosArr, disposable] = createVertexMarkers(obj, isActive, ps)

    // create a stream for tap event of the mesh
    const [updateTap, tapped] = createAdapter()
    // pipe mesh tap events into the new tapped stream
    let updateTapDispose = mesh.tapEvents.run(mkSink(updateTap), scheduler)

    // update roof corner positions and the mesh
    const updatePos = (arr: Vector2[]) => {
        // remove old mesh and dispose old tap event pipe
        obj.remove(mesh)
        dispose(updateTapDispose)

        // create new mesh and setup the tap event pipe
        mesh = createRoofMesh(arr, true)
        obj.add(mesh)
        updateTapDispose = mesh.tapEvents.run(mkSink(updateTap), scheduler)
    }
    const disposable2 = newPosArr.run(mkSink(updatePos), scheduler)

    const toWorld = (v: Vector2): Vector3 => {
        return obj.localToWorld(new Vector3(v.x, v.y, 0))
    }
    const newRoofs = map(
        compose(
            updateRoofPlate(roof),
            fmap(toWorld)
        ),
        newPosArr
    )

    return {
        roof: multicast(debounce(1000, newRoofs)),
        tapped: tapped,
        roofObject: obj,
        disposable: disposeAll([
            disposable,
            disposable1,
            disposable2,
            updateTapDispose
        ])
    }
}
