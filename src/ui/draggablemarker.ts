import {
    Vector3,
    Vector2,
    CircleGeometry,
    MeshBasicMaterial,
    Object3D
} from 'three'
import { Stream, Disposable } from '@most/types'
import { scan, multicast, merge, skipRepeats, map } from '@most/core'
import memoizeWith from 'ramda/es/memoizeWith'
import always from 'ramda/es/always'
import { DraggableMesh, calcDragDelta } from '../custom/mesh'
import { mkSink } from '../sink'
import { defScheduler } from '../helper'
import curry from 'ramda/es/curry'
import clone from 'ramda/es/clone'
import { SceneDragEvent } from '../sceneevent'
import { DragType } from '../input'

/**
 * defines all data for a Draggable Marker
 */
export interface DraggableMarker {
    marker: Object3D
    position: Stream<Vector3>
    isDragging: Stream<boolean>
    disposable: Disposable
}

// get the marker material. This function is memoized so the material is
// only created once and shared.
const getMarkerMaterial = memoizeWith(always('marker_material'), () => {
    return new MeshBasicMaterial({ color: 0xff2222 })
})

function createVisibleMarker(): DraggableMesh {
    const geo = new CircleGeometry(0.5, 32)
    const mat = getMarkerMaterial()

    return new DraggableMesh(geo, mat)
}

// get invisible material for the big circle under marker to ease dragging.
// this function is memoized so the material is created only once and shared.
const getInvisibleMaterial = memoizeWith(always('invisible_material'), () => {
    const mat = new MeshBasicMaterial()
    mat.transparent = true
    mat.opacity = 0.01

    return mat
})

function createInvisibleCircle(): DraggableMesh {
    const geo = new CircleGeometry(10, 32)
    const mat = getInvisibleMaterial()

    return new DraggableMesh(geo, mat)
}

/**
 * create a draggable marker.
 * @param active a stream signalling if the current roof is active or not
 * @param position 2D start position of the marker
 * @returns DraggableMarker
 */
export const createDraggableMarker = curry(
    (active: Stream<boolean>, position: Vector2): DraggableMarker => {
        const markerObj = new Object3D()
        markerObj.name = 'drag-marker'

        // create the visible marker
        const mesh = createVisibleMarker()

        const defPosition = new Vector3(position.x, position.y, 0.1)
        mesh.position.copy(defPosition)
        mesh.visible = false
        markerObj.add(mesh)

        // create the invisible circle
        const invCircle = createInvisibleCircle()
        invCircle.position.copy(defPosition)
        invCircle.visible = false
        invCircle.renderOrder = 10
        markerObj.add(invCircle)

        const disposable = active.run(
            mkSink(a => {
                mesh.visible = a
            }),
            defScheduler()
        )

        const evts = multicast(merge(mesh.dragEvents, invCircle.dragEvents))
        const isDragging = (e: SceneDragEvent): boolean => {
            return e.type == DragType.DragStart || e.type == DragType.Drag
        }
        const dragging = skipRepeats(map(isDragging, evts))

        dragging.run(
            mkSink(d => {
                invCircle.visible = d
            }),
            defScheduler()
        )

        const delta = calcDragDelta(evts, v => {
            const obj = markerObj.parent
            return obj == null ? null : obj.worldToLocal(v)
        })

        // function to update the mesh position based on dragDelta
        const updatePos = (lastPos: Vector3, delta: Vector3): Vector3 => {
            const np = clone(lastPos)
            const nDelta = new Vector3(delta.x, delta.y, 0)
            np.add(nDelta)
            mesh.position.copy(np)
            invCircle.position.copy(np)

            return np
        }

        const newPos = scan(updatePos, defPosition, delta)

        return {
            marker: markerObj,
            position: multicast(newPos),
            isDragging: multicast(dragging),
            disposable: disposable
        }
    }
)
