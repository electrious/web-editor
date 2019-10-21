import { RoofPlate } from './models/roofplate'
import {
    Mesh,
    Object3D,
    Vector2,
    Shape,
    ShapeGeometry,
    MeshBasicMaterial
} from 'three'

export interface RoofNode {
    roof: RoofPlate
    roofObject: Object3D
}

// create roof mesh
function createRoofMesh(roof: RoofPlate, obj: Object3D): Mesh {
    // convert all roof border points to local coordinate
    // and get only the x,y coordinates
    const ps = roof.borderPoints.map(p => {
        const np = obj.worldToLocal(p)
        return new Vector2(np.x, np.y)
    })

    // create a ShapeGeometry with the Shape from border points
    const shp = new Shape(ps)
    const shpGeo = new ShapeGeometry(shp)

    // setup the material
    const mat = new MeshBasicMaterial({ color: 0xffffbb })
    mat.transparent = true
    mat.opacity = 0.7

    return new Mesh(shpGeo, mat)
}

/**
 * create RoofNode for a RoofPlate
 * @param roof
 */
export function createRoofNode(roof: RoofPlate): RoofNode {
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

    return {
        roof: roof,
        roofObject: obj
    }
}
