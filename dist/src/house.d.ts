import { Object3D, Mesh, Material, BufferGeometry, Geometry } from 'three';
import { Stream } from '@most/types';
import RBush from 'rbush';
import { VertexItem } from './algorithm/meshflatten';
import { MouseMovable, SceneMouseMoveEvent, Tappable, SceneTapEvent } from './sceneevent';
/**
 * HouseMesh is a Mesh subclass with support for mouse move event and allow user
 * to add new roofs by moving mouse over the mesh.
 */
declare class HouseMesh extends Mesh implements MouseMovable, Tappable {
    private canAcceptMouseMove;
    tappedEvent: Stream<SceneTapEvent>;
    private tapFunc;
    mouseMoveEvent: Stream<SceneMouseMoveEvent>;
    private mouseMoved;
    constructor(geo: Geometry | BufferGeometry, mat: Material);
    enableAddingRoof(enabled: boolean): void;
    tapped(event: SceneTapEvent): void;
    mouseMove(event: SceneMouseMoveEvent): void;
}
/**
 * Load the house mesh of the specified lead.
 * @param {number} leadId
 */
export declare function loadHouse(leadId: number): Stream<Object3D>;
/**
 * House mesh data, including the mesh, the original geometry, and the vertices
 * rtree built from all vertices
 */
export interface HouseMeshData {
    wrapper: Object3D;
    mesh: HouseMesh;
    geometry: BufferGeometry;
    verticeTree: RBush<VertexItem>;
}
/**
 * get the HouseMeshData for the house object loaded.
 * @param obj
 */
export declare function getHouseMeshData(obj: Object3D): HouseMeshData | null;
export {};
