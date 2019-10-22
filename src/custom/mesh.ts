import { Mesh, Geometry, Material } from 'three'
import { RaycastTarget, SceneEvent } from '../sceneevent'

export class SceneEventMesh extends Mesh implements RaycastTarget {
    constructor(geo: Geometry, mat: Material) {
        super(geo, mat)
    }

    processEvent(event: SceneEvent): boolean {
        console.log(event)

        return false
    }
}
