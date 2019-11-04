import {
    Vector3,
    Vector2,
    CircleGeometry,
    MeshBasicMaterial,
    Object3D,
    Geometry,
    Material
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
 * defines all data for a Draggable Object
 */
export interface DraggableObject {
    object: Object3D
    position: Stream<Vector3>
    isDragging: Stream<boolean>
    disposable: Disposable
}

// get the default material. This function is memoized so the material is
// only created once and shared.
const getMaterial = memoizeWith(always('marker_material'), () => {
    return new MeshBasicMaterial({ color: 0xff2222 })
})

// create the visible part of the object, user can specify custom geometry and
// material
function createVisibleObject(
    customGeo?: Geometry | undefined,
    customMat?: Material | undefined
): DraggableMesh {
    const geo = customGeo == undefined ? new CircleGeometry(0.5, 32) : customGeo
    const mat = customMat == undefined ? getMaterial() : customMat

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
 * create a draggable object.
 * @param active a stream signalling if the current roof is active or not
 * @param position 2D start position of the object
 * @param customGeo optional custom geometry for the object
 * @param customMat optional custom material for the object
 * @returns DraggableObject
 */
export const createDraggableObject = (
    active: Stream<boolean>,
    position: Vector2,
    customGeo?: Geometry | undefined,
    customMat?: Material | undefined
): DraggableObject => {
    const dragObj = new Object3D()
    dragObj.name = 'drag-object'

    // create the visible marker
    const mesh = createVisibleObject(customGeo, customMat)

    const defPosition = new Vector3(position.x, position.y, 0.1)
    mesh.position.copy(defPosition)
    mesh.visible = false
    dragObj.add(mesh)

    // create the invisible circle
    const invCircle = createInvisibleCircle()
    invCircle.position.copy(defPosition)
    invCircle.visible = false
    invCircle.renderOrder = 10
    dragObj.add(invCircle)

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
        const obj = dragObj.parent
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
        object: dragObj,
        position: multicast(newPos),
        isDragging: multicast(dragging),
        disposable: disposable
    }
}
