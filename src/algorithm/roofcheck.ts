import { Object3D, Vector2, Vector3 } from 'three'
import { SceneMouseMoveEvent } from '../sceneevent'
import { RoofPlate, getRoofPolygon } from '../models/roofplate'
import any from 'ramda/es/any'
import { pointInPolygon } from './pointinpolygon'
import { Angle } from '../math/angle'

/**
 * check if there can be a roof at the point under mouse
 * @param house
 * @param roofs
 * @param e
 */
export function couldBeRoof(
    house: Object3D,
    roofs: RoofPlate[],
    e: SceneMouseMoveEvent
): boolean {
    const roofPoly = roofs.map(getRoofPolygon)

    // get the local coordinate of the intersection point
    // in the house mesh.
    const localPoint = house.worldToLocal(e.point)

    // 2D projection of the intersection point
    const flatP = new Vector2(localPoint.x, localPoint.y)

    // check if the point is under any roof
    const underRoof = any(poly => pointInPolygon(poly, flatP), roofPoly)
    if (underRoof) return false

    // check if the intersected face has a reasonable slope
    const norm = e.face.normal
    const up = new Vector3(0, 0, 1)

    const angle = Angle.fromRad(Math.acos(up.dot(norm)))

    return angle.deg < 60
}
