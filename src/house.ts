import { OBJLoader2Parallel } from 'three/examples/jsm/loaders/OBJLoader2Parallel'
import {
    MTLLoader,
    MaterialCreator
} from 'three/examples/jsm/loaders/MTLLoader'
import { Object3D, Mesh, Material } from 'three'
import { Stream } from '@most/types'
import { fromPromise } from '@most/core'

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
