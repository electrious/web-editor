import { OBJLoader2Parallel } from 'three/examples/jsm/loaders/OBJLoader2Parallel'
import {
    MTLLoader,
    MaterialCreator
} from 'three/examples/jsm/loaders/MTLLoader'
import { Object3D, Mesh, Material, Vector3, BufferGeometry } from 'three'
import { Stream } from '@most/types'
import { fromPromise } from '@most/core'
import find from 'ramda/es/find'
import RBush from 'rbush'
import { VertexItem, buildRTree } from './algorithm/meshflatten'

function meshPath(leadId: number): string {
    return (
        'https://s3.eu-west-1.amazonaws.com/data.electrious.com/leads/' +
        leadId +
        '/mesh/'
    )
}

/**
 * Apply the material creator's material to the Mesh, which is a child of
 * the loaded Object3D.
 * @param matCreator
 * @param obj
 */
function applyMaterialCreator(matCreator: MaterialCreator, obj: Object3D) {
    const mat = matCreator.materials.scene
    mat.transparent = false

    obj.children.forEach(child => {
        if (child instanceof Mesh) {
            child.name = '3dmap'

            if (child.material instanceof Material) {
                child.material.dispose()
            }

            child.material = mat
        }
    })
}

/**
 * Load the house mesh of the specified lead.
 * @param {number} leadId
 */
export function loadHouse(leadId: number): Stream<Object3D> {
    const objLoader = new OBJLoader2Parallel()
    const mtlLoader = new MTLLoader()
    const path = meshPath(leadId)

    return fromPromise(
        new Promise(resolve => {
            mtlLoader.setPath(path)
            mtlLoader.load('scene.mtl', materials => {
                materials.preload()

                objLoader.load(path + 'scene.obj', object => {
                    object.name = 'house-mesh'
                    const childs = object.children
                    childs.forEach(c => {
                        c.castShadow = true
                        c.receiveShadow = true
                    })
                    applyMaterialCreator(materials, object)

                    resolve(object)
                })
            })
        })
    )
}

/**
 * House mesh data, including the mesh, the original geometry, and the vertices
 * rtree built from all vertices
 */
export interface HouseMeshData {
    mesh: Mesh
    geometry: BufferGeometry
    verticeTree: RBush<VertexItem>
}

/**
 * get the house mesh from the loaded Object3D
 * @param obj
 */
function getHouseMesh(obj: Object3D): Mesh | undefined {
    const m = find(m => m.name == '3dmap', obj.children)
    if (m == undefined) return undefined

    return m as Mesh
}

/**
 * get the vertex position and normal vector arrays of a BufferGeometry
 * @param mesh
 */
function getGeometryInfo(mesh: Mesh) {
    const geo = mesh.geometry.clone()

    if (geo instanceof BufferGeometry) {
        const posAttr = geo.getAttribute('position')
        const normAttr = geo.getAttribute('normal')

        const posVecs = []

        for (let i = 0; i < posAttr.count; i++) {
            const x = posAttr.getX(i)
            const y = posAttr.getY(i)
            const z = posAttr.getZ(i)

            posVecs.push(new Vector3(x, y, z))
        }

        const normVecs = []

        for (let i = 0; i < normAttr.count; i++) {
            const x = normAttr.getX(i)
            const y = normAttr.getY(i)
            const z = normAttr.getZ(i)

            const n = new Vector3(x, y, z)
            n.normalize()
            normVecs.push(n)
        }

        return { geometry: geo, vertices: posVecs, normals: posVecs }
    }

    return null
}

/**
 * get the HouseMeshData for the house object loaded.
 * @param obj
 */
export function getHouseMeshData(obj: Object3D): HouseMeshData | null {
    const mesh = getHouseMesh(obj)
    if (mesh == undefined) return null

    const d = getGeometryInfo(mesh)
    if (d == null) return null

    const tree = buildRTree(d.vertices, d.normals)

    return { mesh: mesh, geometry: d.geometry, verticeTree: tree }
}
