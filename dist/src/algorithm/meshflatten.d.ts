import { Vector3, Mesh, BufferGeometry } from 'three';
import { RoofPlate } from '../models/roofplate';
import RBush from 'rbush';
/**
 * Vertex data that will be inserted into RTree
 */
export interface VertexItem {
    minX: number;
    minY: number;
    maxX: number;
    maxY: number;
    vertex: Vector3;
    normal: Vector3;
    index: number;
}
/**
 * Build an RTree from a list of vertices
 * @param vertices
 */
export declare function buildRTree(vertices: Vector3[], normals: Vector3[]): RBush<VertexItem>;
/**
 * Flatten all roofplates
 * @param geo original BufferGeometry of the house mesh
 * @param tree an RTree with all vertices of the house mesh
 * @param house the House Mesh
 * @param roofs all roofplates and their Object3D node in the scene graph
 */
export declare function flattenRoofPlates(geo: BufferGeometry, tree: RBush<VertexItem>, house: Mesh, roofs: RoofPlate[]): void;
