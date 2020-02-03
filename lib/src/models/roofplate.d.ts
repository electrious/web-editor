import { Vector3, Vector2 } from 'three';
import { Angle } from '../math/angle';
export declare enum Orientation {
    Landscape = 0,
    Portrait = 1
}
export declare enum Alignment {
    Grid = 0,
    Brick = 1
}
export interface RoofPlate {
    id: string;
    intId: number;
    leadId: number;
    borderPoints: Vector3[];
    coefs: number[];
    center: Vector3;
    normal: Vector3;
    orientation: Orientation;
    alignment: Alignment;
    slope: Angle;
    azimuth: Angle;
    rotation: Angle;
}
export declare function cloneRoof(roof: RoofPlate): RoofPlate;
export interface JSRoofPlate {
    uuid: string;
    id: number;
    lead_id: number;
    border_points: {
        x: number;
        y: number;
        z: number;
    }[];
    coefs: number[];
    center: number[];
    normal: number[];
    orientation: number;
    alignment: number;
    slope: number;
    azimuth: number;
    rotation_override: number;
}
/**
 * convert an external JSRoofPlate object to internal RoofPlate model
 * @param r
 */
export declare function fromJSRoofPlate(r: JSRoofPlate): RoofPlate;
/**
 * 2D Polygon for roof plate projection on ground
 */
export declare type Polygon = Vector2[];
/**
 * get the 2D polygon for a roof plate
 * @param roof
 */
export declare function getRoofPolygon(roof: RoofPlate): Polygon;
/**
 * create a new RoofPlate based on the start position and normal vector.
 * @param center
 * @param normal
 */
export declare function newRoofPlate(center: Vector3, normal: Vector3): RoofPlate;
/**
 * enum that defines the types of operation applied to roofs
 */
export declare enum RoofOperationType {
    Create = 0,
    Delete = 1,
    Update = 2
}
/**
 * interface for objects that encode the operations on a roof.
 */
export interface RoofOperation {
    type: RoofOperationType;
    roof: RoofPlate | string;
}
export declare function mkCreateRoofOp(roof: RoofPlate): RoofOperation;
export declare function mkDeleteRoofOp(roof: RoofPlate): RoofOperation;
export declare function mkUpdateRoofOp(roof: RoofPlate): RoofOperation;
export interface Point {
    x: number;
    y: number;
    z: number;
}
export interface RoofEdited {
    ground: Point;
    inclined: Point;
    contours: Point[];
    indices: number[];
}
export declare function toRoofsEdited(roofs: RoofPlate[]): RoofEdited[];
