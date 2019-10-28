import { Vector3, Vector2, CircleGeometry, MeshBasicMaterial } from 'three'
import { Stream, Disposable } from '@most/types'
import { scan, multicast } from '@most/core'
import memoizeWith from 'ramda/es/memoizeWith'
import always from 'ramda/es/always'
import { DraggableMesh } from '../custom/mesh'
import { mkSink } from '../sink'
import { defScheduler } from '../helper'
import curry from 'ramda/es/curry'
import clone from 'ramda/es/clone'

/**
 * defines all data for a Draggable Marker
 */
export interface DraggableMarker {
    marker: DraggableMesh
    position: Stream<Vector3>
    disposable: Disposable
}

// get the marker material. This function is memoized so the material is
// only created once and shared.
const getMarkerMaterial = memoizeWith(always('marker_material'), () => {
    return new MeshBasicMaterial({ color: 0xff2222 })
})

/**
 * create a draggable marker.
 * @param active a stream signalling if the current roof is active or not
 * @param position 2D start position of the marker
 * @returns DraggableMarker
 */
export const createDraggableMarker = curry(
    (active: Stream<boolean>, position: Vector2): DraggableMarker => {
        const geo = new CircleGeometry(1, 32)
        const mat = getMarkerMaterial()

        const mesh = new DraggableMesh(geo, mat)
        const defPosition = new Vector3(position.x, position.y, 0.1)
        mesh.position.copy(defPosition)
        mesh.visible = false

        const disposable = active.run(
            mkSink(a => {
                mesh.visible = a
            }),
            defScheduler()
        )

        // function to update the mesh position based on dragDelta
        const updatePos = (lastPos: Vector3, delta: Vector3): Vector3 => {
            const np = clone(lastPos)
            const nDelta = new Vector3(delta.x, delta.y, 0)
            np.add(nDelta)
            mesh.position.copy(np)

            return np
        }

        const newPos = scan(updatePos, defPosition, mesh.dragDelta)

        newPos.run(mkSink(), defScheduler())

        return {
            marker: mesh,
            position: multicast(newPos),
            disposable: disposable
        }
    }
)
