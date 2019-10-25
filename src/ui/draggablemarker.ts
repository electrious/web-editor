import {
    Mesh,
    Vector3,
    Vector2,
    CircleGeometry,
    MeshBasicMaterial
} from 'three'
import { Stream, Disposable } from '@most/types'
import { empty } from '@most/core'
import memoizeWith from 'ramda/es/memoizeWith'
import always from 'ramda/es/always'
import { DraggableMesh } from '../custom/mesh'
import { mkSink } from '../sink'
import { defScheduler } from '../helper'
import curry from 'ramda/es/curry'

export interface DraggableMarker {
    marker: DraggableMesh
    position: Stream<Vector3>
    disposable: Disposable
}

const getMarkerMaterial = memoizeWith(always('marker_material'), () => {
    return new MeshBasicMaterial({ color: 0xff2222 })
})

export const createDraggableMarker = curry(
    (active: Stream<boolean>, position: Vector2): DraggableMarker => {
        const geo = new CircleGeometry(0.3, 32)
        const mat = getMarkerMaterial()

        const mesh = new DraggableMesh(geo, mat)
        mesh.position.set(position.x, position.y, 0.1)
        mesh.visible = false

        const disposable = active.run(
            mkSink(a => {
                mesh.visible = a
            }),
            defScheduler()
        )

        return {
            marker: mesh,
            position: empty(),
            disposable: disposable
        }
    }
)
