import { Vector3 } from 'three'
import { Angle } from '../math/angle'
import map from 'ramda/es/map'

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
