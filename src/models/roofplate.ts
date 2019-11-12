import { Vector3, Vector2 } from 'three'
import { Angle } from '../math/angle'

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
