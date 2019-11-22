import { Vector3, Mesh, Vector2, BufferGeometry, BufferAttribute } from 'three'
import { RoofPlate, Polygon, getRoofPolygon } from '../models/roofplate'
import RBush, { BBox } from 'rbush'
import { pointInPolygon } from './pointinpolygon'
import flatten from 'ramda/es/flatten'
import { Angle } from '../math/angle'

/**
 * Vertex data that will be inserted into RTree
 */
export interface VertexItem {
    minX: number
    minY: number
    maxX: number
    maxY: number
    vertex: Vector3 // vertex position
    normal: Vector3 // vertex normal
    index: number // vertex index in the BufferGeometry
}

// offset used to calculate bounding box for a point
const vertexOffset = 0.0001

/**
 * calculate VertexItem for a vertex point
 * @param point
 */
function vertexItem(
    point: Vector3,
    normal: Vector3,
    index: number
): VertexItem {
    const x = point.x
    const y = point.y

    return {
        minX: x - vertexOffset,
        maxX: x + vertexOffset,
        minY: y - vertexOffset,
        maxY: y + vertexOffset,
        vertex: point,
        normal: normal,
        index: index
    }
}

/**
 * Build an RTree from a list of vertices
 * @param vertices
 */
export function buildRTree(
    vertices: Vector3[],
    normals: Vector3[]
): RBush<VertexItem> {
    const tree = new RBush<VertexItem>()

    const items = []
    for (let i = 0; i < vertices.length; i++) {
        const v = vertices[i]
        const n = normals[i]
        items.push(vertexItem(v, n, i))
    }
    tree.load(items)

    return tree
}

/**
 * get the bounding box of a polygon
 * @param p
 */
function polygonBoundingBox(polygon: Polygon): BBox {
    let minX = Infinity
    let minY = Infinity
    let maxX = -Infinity
    let maxY = -Infinity

    for (const v of polygon) {
        if (v.x < minX) minX = v.x
        if (v.x > maxX) maxX = v.x
        if (v.y < minY) minY = v.y
        if (v.y > maxY) maxY = v.y
    }

    return { minX: minX, minY: minY, maxX: maxX, maxY: maxY }
}

/**
 * Internal helper class to do flattening on a roof
 */
class RoofFlattener {
    roofNormal: Vector3
    roofCenter: Vector3

    roofPolygon: Polygon

    constructor(normal: Vector3, center: Vector3, polygon: Polygon) {
        this.roofNormal = normal
        this.roofCenter = center
        this.roofPolygon = polygon
    }

    /**
     * flatten a vertex, returns a new position for that vertex
     * @param v
     */
    flatten(v: Vector3): Vector3 {
        const nv = this.roofCenter.clone()
        nv.addScaledVector(v, -1)
        const scale = this.roofNormal.dot(nv)

        const r = v.clone()
        r.addScaledVector(this.roofNormal, scale)

        return r
    }

    /**
     * calculate distance from the param position to the roof.
     * @param v
     */
    distToRoof(v: Vector3): number {
        const nv = v.clone()
        nv.sub(this.roofCenter)

        return this.roofNormal.dot(nv)
    }
}

/**
 * get the RoofFlattener for a roof
 * @param roof
 */
function roofFlattener(roof: RoofPlate): RoofFlattener {
    const poly = getRoofPolygon(roof)

    return new RoofFlattener(roof.normal, roof.center, poly)
}

/**
 * flattened vertex info
 */
interface FlattenedVertex {
    index: number
    newPos: Vector3
}

/**
 * apply the flattened vertices to the BufferGeometry and return a new one
 * @param geo
 * @param fvs
 */
function applyFlattendVertex(
    geo: BufferGeometry,
    fvs: FlattenedVertex[]
): BufferGeometry {
    const newGeo = geo.clone()
    const attr = newGeo.getAttribute('position')

    if (attr instanceof BufferAttribute) {
        for (const fv of fvs) {
            const p = fv.newPos
            attr.setXYZ(fv.index, p.x, p.y, p.z)
        }
        attr.needsUpdate = true

        return newGeo
    }
    return geo
}

/**
 * Flatten a single roof plate
 * @param tree
 * @param roof
 */
function flattenRoofplate(
    tree: RBush<VertexItem>,
    roof: RoofPlate
): FlattenedVertex[] {
    const flattener = roofFlattener(roof)
    const poly = flattener.roofPolygon
    // search for all vertices under the roof polygon bounding box
    const candidates = tree.search(polygonBoundingBox(poly))

    // for all candidates, check if it's inside the polygon.
    // if it is, then flatten that vertex
    const result: FlattenedVertex[] = []
    for (const cand of candidates) {
        const vec2 = new Vector2(cand.vertex.x, cand.vertex.y)
        if (pointInPolygon(poly, vec2)) {
            // now, the point is inside the roof polygon. check its distance
            // to the roof and the angle between its normal vector with the
            // roof normal vector.
            const angle = Angle.fromRad(
                Math.acos(flattener.roofNormal.dot(cand.normal))
            )
            const dist = flattener.distToRoof(cand.vertex)

            // only add points that're close to the roofplate and within
            // an angle limit to flattening result.
            if (
                (dist < 0.5 && dist >= 0) ||
                (dist < 0 && dist > -1 && angle.deg < 20)
            ) {
                const newPos = flattener.flatten(cand.vertex)
                result.push({ index: cand.index, newPos: newPos })
            }
        }
    }

    return result
}

/**
 * Flatten all roofplates
 * @param geo original BufferGeometry of the house mesh
 * @param tree an RTree with all vertices of the house mesh
 * @param house the House Mesh
 * @param roofs all roofplates and their Object3D node in the scene graph
 */
export function flattenRoofPlates(
    geo: BufferGeometry,
    tree: RBush<VertexItem>,
    house: Mesh,
    roofs: RoofPlate[]
) {
    const fvs = flatten(roofs.map(r => flattenRoofplate(tree, r)))

    const newGeo = applyFlattendVertex(geo, fvs)

    // apply the new geometry
    house.geometry = newGeo
}
