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

// RoofPlate model used in JS code. The data received from user and updates
// sent back to user should be in this format.
export interface JSRoofPlate {
    uuid: string
    id: number
    lead_id: number
    border_points: { x: number; y: number; z: number }[]
    coefs: number[]
    center: number[]
    normal: number[]
    orientation: number
    alignment: number
    slope: number
    azimuth: number
    rotation_override: number
}

const mkVec = (ns: number[]) => {
    return new Vector3(ns[0], ns[1], ns[2])
}

const fromVec = (v: Vector3) => {
    return [v.x, v.y, v.z]
}

/**
 * convert an external JSRoofPlate object to internal RoofPlate model
 * @param r
 */
export function fromJSRoofPlate(r: JSRoofPlate): RoofPlate {
    return {
        id: r.uuid,
        intId: r.id,
        leadId: r.lead_id,
        borderPoints: r.border_points.map(v => {
            return new Vector3(v.x, v.y, v.z)
        }),
        coefs: r.coefs,
        center: mkVec(r.center),
        normal: mkVec(r.normal),
        orientation: r.orientation,
        alignment: r.alignment,
        slope: new Angle(r.slope),
        azimuth: new Angle(r.azimuth),
        rotation: new Angle(r.rotation_override)
    }
}

/**
 * convert an internal RoofPlate object to an external JSRoofPlate object
 * @param r
 */
export function toJSRoofPlate(r: RoofPlate): JSRoofPlate {
    return {
        uuid: r.id,
        id: r.intId,
        // eslint-disable-next-line @typescript-eslint/camelcase
        lead_id: r.leadId,
        // eslint-disable-next-line @typescript-eslint/camelcase
        border_points: r.borderPoints.map(v => {
            return { x: v.x, y: v.y, z: v.z }
        }),
        coefs: r.coefs,
        center: fromVec(r.center),
        normal: fromVec(r.normal),
        orientation: r.orientation,
        alignment: r.alignment,
        slope: r.slope.deg,
        azimuth: r.azimuth.deg,
        // eslint-disable-next-line @typescript-eslint/camelcase
        rotation_override: r.rotation.deg
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
 * calculate the azimuth angle based on roof normal
 * @param normal
 */
function getAzimuth(normal: Vector3): Angle {
    const a = Angle.fromRad(Math.atan2(normal.x, normal.y))

    if (a.deg > 360) {
        return new Angle(a.deg - 360)
    } else if (a.deg < 0) {
        return new Angle(a.deg + 360)
    } else return a
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

    const azimuth = getAzimuth(normal)

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
        center: center.clone(),
        normal: normal.clone(),
        orientation: Orientation.Landscape,
        alignment: Alignment.Brick,
        slope: slope,
        azimuth: azimuth,
        rotation: new Angle(0)
    }
}

/**
 * enum that defines the types of operation applied to roofs
 */
export enum RoofOperationType {
    Create,
    Delete,
    Update
}

/**
 * interface for objects that encode the operations on a roof.
 */
export interface RoofOperation {
    type: RoofOperationType
    roof: RoofPlate | string
}

function isRoofPlate(r: RoofPlate | string): r is RoofPlate {
    return (r as RoofPlate).normal !== undefined
}

/**
 * interface for objects that encode the operations on a roof with
 * an external JSRoofPlate object
 */
export interface JSRoofOperation {
    type: RoofOperationType
    roof: JSRoofPlate | string
}

export function mkCreateRoofOp(roof: RoofPlate): RoofOperation {
    return {
        type: RoofOperationType.Create,
        roof: roof
    }
}

export function mkDeleteRoofOp(roof: RoofPlate): RoofOperation {
    return {
        type: RoofOperationType.Delete,
        roof: roof.id
    }
}

export function mkUpdateRoofOp(roof: RoofPlate): RoofOperation {
    return {
        type: RoofOperationType.Update,
        roof: roof
    }
}

export function toJSRoofOperation(o: RoofOperation): JSRoofOperation {
    const r = o.roof
    if (isRoofPlate(r)) {
        return { type: o.type, roof: toJSRoofPlate(r) }
    } else {
        return { type: o.type, roof: r }
    }
}
