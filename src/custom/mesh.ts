import { Mesh, Geometry, Material } from 'three'
import {
    Tappable,
    SceneTapEvent,
    Draggable,
    SceneDragEvent
} from '../sceneevent'

export class TappableMesh extends Mesh implements Tappable {
    constructor(geo: Geometry, mat: Material) {
        super(geo, mat)
    }

    tapped(event: SceneTapEvent): boolean {
        console.log(event)

        return false
    }
}

export class DraggableMesh extends Mesh implements Draggable {
    constructor(geo: Geometry, mat: Material) {
        super(geo, mat)
    }

    dragged(event: SceneDragEvent): boolean {
        console.log(event)

        return false
    }
}
