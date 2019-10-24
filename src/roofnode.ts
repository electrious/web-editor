import { RoofPlate } from './models/roofplate'
import {
    Object3D,
    Vector2,
    Shape,
    ShapeGeometry,
    MeshBasicMaterial
} from 'three'
import { TappableMesh } from './custom/mesh'
import memoizeWith from 'ramda/es/memoizeWith'
import always from 'ramda/es/always'
import { Stream } from '@most/types'
import { SceneTapEvent } from './sceneevent'
import { mkSink } from './sink'
import { defScheduler } from './helper'

export interface RoofNode {
    roof: RoofPlate
    tapped: Stream<SceneTapEvent>
    roofObject: Object3D
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
function createRoofMesh(roof: RoofPlate, obj: Object3D): TappableMesh {
    // convert all roof border points to local coordinate
    // and get only the x,y coordinates
    const ps = roof.borderPoints.map(p => {
        const np = obj.worldToLocal(p)
        return new Vector2(np.x, np.y)
    })

    // create a ShapeGeometry with the Shape from border points
    const shp = new Shape(ps)

    return new TappableMesh(new ShapeGeometry(shp), getDefMaterial())
}

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
    // position and rotation changed, so that createRoofMesh can use them
    // to convert coordinates correctly
    obj.updateMatrix()
    obj.updateMatrixWorld()

    const mesh = createRoofMesh(roof, obj)
    obj.add(mesh)

    const activate = (active: boolean) => {
        mesh.material = active ? getActiveMaterial() : getDefMaterial()
    }

    isActive.run(mkSink(activate), defScheduler())

    return {
        roof: roof,
        tapped: mesh.tapEvents,
        roofObject: obj
    }
}
