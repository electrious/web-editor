import { Vector3, Vector2 } from 'three'
import { Angle } from '../math/angle'
import map from 'ramda/es/map'
import uuidv4 from 'uuid/v4'

export enum Orientation {
    Landscape,
    Portrait
}

export enum Alignment {
    Grid,
    Brick
}

// RoofPlate model object interface
export interface RoofPlate {
    id: string
    intId: number
    leadId: number
    borderPoints: Vector3[]
    coefs: number[]
    center: Vector3
    normal: Vector3
    orientation: Orientation
    alignment: Alignment
    slope: Angle
    azimuth: Angle
    rotation: Angle
}

export function cloneRoof(roof: RoofPlate): RoofPlate {
    return {
        id: roof.id,
        intId: roof.intId,
        leadId: roof.leadId,
        borderPoints: map(v => v.clone(), roof.borderPoints),
        coefs: roof.coefs,
        center: roof.center.clone(),
        normal: roof.normal.clone(),
        orientation: roof.orientation,
        alignment: roof.alignment,
        slope: roof.slope,
        azimuth: roof.azimuth,
        rotation: roof.rotation
    }
}

/**
 * 2D Polygon for roof plate projection on ground
 */
export type Polygon = Vector2[]

/**
 * get the 2D polygon for a roof plate
 * @param roof
 */
export function getRoofPolygon(roof: RoofPlate): Polygon {
    return roof.borderPoints.map(v => new Vector2(v.x, v.y))
}

/**
 * helper function to calculate angle between two Vector3
 * @param v1
 * @param v2
 */
function angleBetween(v1: Vector3, v2: Vector3): Angle {
    const d = v1.dot(v2)

    const angle = Math.acos(d / (v1.length() * v2.length()))
    return Angle.fromRad(angle)
}

/**
 * calculate the gutter vector based on roof normal vector
 * @param normal
 */
function gutterVector(normal: Vector3): Vector3 {
    // the gutter vector always has 0 for the z element, and it should be
    // perpendicular to the normal vector.
    // Assume normal vector to be (nx, ny, nz)
    // and let the gutter vector to be (x, y, 0).
    // First, the dot product should be 0, so 'nx * x + ny * y + nz * 0 = 0'
    // Second, let it be normalized vector, so 'x * x + y * y + 0 = 1'
    // solve the two equations and get the x,y (we only need one solution, and
    // we'll take the positive x here.)
    const nx = normal.x
    const ny = normal.y

    const c = nx / ny
    const x = Math.sqrt(1 / (1 + c * c))
    const y = -x * c

    return new Vector3(x, y, 0)
}

/**
 * get the rafter vector based on normal vector and gutter vector.
 * @param normal
 * @param gutter
 */
function rafterVector(normal: Vector3, gutter: Vector3): Vector3 {
    const r = new Vector3(0, 0, 0)
    r.crossVectors(normal, gutter)

    return r
}

function defBorderPoints(
    center: Vector3,
    gutter: Vector3,
    rafter: Vector3
): Vector3[] {
    const m1 = center.clone()
    m1.addScaledVector(gutter, 2)
    const m2 = center.clone()
    m2.addScaledVector(gutter, -2)

    const p1 = m1.clone()
    p1.addScaledVector(rafter, 2)

    const p2 = m1.clone()
    p2.addScaledVector(rafter, -2)

    const p3 = m2.clone()
    p3.addScaledVector(rafter, -2)

    const p4 = m2.clone()
    p4.addScaledVector(rafter, 2)

    return [p1, p2, p3, p4, p1]
}

/**
 * create a new RoofPlate based on the start position and normal vector.
 * @param center
 * @param normal
 */
export function newRoofPlate(center: Vector3, normal: Vector3): RoofPlate {
    // normal vector projection on ground
    const projN = new Vector3(normal.x, normal.y, 0)
    const angle = angleBetween(normal, projN)

    const slope = new Angle(90 - angle.deg)

    const azimuth = new Angle(Math.atan(normal.y / normal.x))

    // calculate the gutter and rafter vectors and add default border points
    const gutter = gutterVector(normal)
    const rafter = rafterVector(normal, gutter)
    const borderPoints = defBorderPoints(center, gutter, rafter)

    return {
        id: uuidv4(),
        intId: 0,
        leadId: 0,
        borderPoints: borderPoints,
        coefs: [],
        center: center,
        normal: normal,
        orientation: Orientation.Landscape,
        alignment: Alignment.Brick,
        slope: slope,
        azimuth: azimuth,
        rotation: new Angle(0)
    }
}
