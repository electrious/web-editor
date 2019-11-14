import { RoofPlate, cloneRoof } from './models/roofplate'
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
import { createRoofEditor } from './editor/roofeditor'
import { disposeAll, dispose } from '@most/disposable'
import { map, multicast, debounce } from '@most/core'
import { createAdapter } from '@most/adapter'
import curry from 'ramda/es/curry'
import compose from 'ramda/es/compose'
import fmap from 'ramda/es/map'
import init from 'ramda/es/init'
import append from 'ramda/es/append'
import head from 'ramda/es/head'

export interface RoofNode {
    roofId: string
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

const updateRoofPlate = curry((roof: RoofPlate, ps: Vector3[]) => {
    const newRoof = cloneRoof(roof)
    // make sure the first and last point are the same
    newRoof.borderPoints = append(head(ps), ps)
    return newRoof
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
    // NOTE: the last point will be dropped here because it's the same with the
    // first one.
    const ps = init(
        fmap(p => {
            const np = p.clone()
            obj.worldToLocal(np)
            return new Vector2(np.x, np.y)
        }, roof.borderPoints)
    )

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
    const editor = createRoofEditor(obj, isActive, ps)

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
    const disposable2 = editor.roofVertices.run(mkSink(updatePos), scheduler)

    const toParent = (v: Vector2): Vector3 => {
        // convert the local position to parent's coordinate
        return new Vector3(v.x, v.y, 0).applyMatrix4(obj.matrix)
    }
    const newRoofs = map(
        compose(
            updateRoofPlate(roof),
            fmap(toParent)
        ),
        editor.roofVertices
    )

    return {
        roofId: roof.id,
        roof: multicast(debounce(1000, newRoofs)),
        tapped: tapped,
        roofObject: obj,
        disposable: disposeAll([
            editor.disposable,
            disposable1,
            disposable2,
            updateTapDispose
        ])
    }
}
