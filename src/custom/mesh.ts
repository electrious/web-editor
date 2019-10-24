import { Mesh, Geometry, Material } from 'three'
import {
    Tappable,
    SceneTapEvent,
    Draggable,
    SceneDragEvent
} from '../sceneevent'
import { Stream } from '@most/types'
import { createAdapter } from '@most/adapter'

/**
 * Extend Mesh with Tappable and provide the tapEvents stream
 */
export class TappableMesh extends Mesh implements Tappable {
    // tap event streams
    tapEvents: Stream<SceneTapEvent>

    // private function to insert new tap events to the stream
    private insertTap: (event: SceneTapEvent) => void

    constructor(geo: Geometry, mat: Material) {
        super(geo, mat)

        const [f, s] = createAdapter()
        this.tapEvents = s
        this.insertTap = f
    }

    tapped(event: SceneTapEvent) {
        this.insertTap(event)
    }
}

/**
 * Extend Mesh with Draggable and provide the dragEvents stream
 */
export class DraggableMesh extends Mesh implements Draggable {
    // drag event streams
    dragEvents: Stream<SceneDragEvent>

    // private function to insert drag events into the stream
    private insertDrag: (event: SceneDragEvent) => void

    constructor(geo: Geometry, mat: Material) {
        super(geo, mat)

        const [f, s] = createAdapter()
        this.dragEvents = s
        this.insertDrag = f
    }

    dragged(event: SceneDragEvent) {
        this.insertDrag(event)
    }
}
