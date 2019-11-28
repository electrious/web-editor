import { Stream, Disposable } from '@most/types'
import { snapshot, map } from '@most/core'
import { RoofPlate, newRoofPlate } from '../models/roofplate'
import { Object3D, Vector3, MeshBasicMaterial, CircleGeometry } from 'three'
import { SceneMouseMoveEvent } from '../sceneevent'
import { couldBeRoof } from '../algorithm/roofcheck'
import memoizeWith from 'ramda/es/memoizeWith'
import always from 'ramda/es/always'
import { TappableMesh } from '../custom/mesh'
import { mkSink } from '../sink'
import { defScheduler } from '../helper'

/**
 * RoofRecognizer wlll be able to let user add new roof
 */
export interface RoofRecognizer {
    marker: Object3D
    addedNewRoof: Stream<RoofPlate>
    disposable: Disposable
}

// candidate point that will allow user to show the adder marker
interface CandidatePoint {
    position: Vector3
    faceNormal: Vector3
}

/**
 * functions to create geometry and material for the marker for adding new roofs
 * These functions are memoized so the objects are shared.
 */
const adderMarkerMaterial = memoizeWith(always('adder-marker-mat'), () => {
    return new MeshBasicMaterial({ color: 0x2222ff })
})

const adderMarkerGeometry = memoizeWith(always('adder-marker-geo'), () => {
    return new CircleGeometry(1, 32)
})

interface RoofAdder {
    marker: TappableMesh
    position: Vector3
    normal: Vector3
}

const createAdderMarker = (): RoofAdder => {
    const geo = adderMarkerGeometry()
    const mat = adderMarkerMaterial()

    const marker = new TappableMesh(geo, mat)
    marker.name = 'add-roof-marker'

    return {
        marker: marker,
        position: new Vector3(0, 0, 0),
        normal: new Vector3(0, 1, 0)
    }
}

/**
 * Create a roof recognizer object.
 * @param houseWrapper the house mesh wrapper object
 * @param roofs a stream of the current roof plates
 * @param mouseMove a stream of mouse move events in the scene
 * @returns RoofRecognizer
 */
export function createRoofRecognizer(
    houseWrapper: Object3D,
    roofs: Stream<RoofPlate[]>,
    mouseMove: Stream<SceneMouseMoveEvent>
): RoofRecognizer {
    // create the adder marker and add it to parent
    const adder = createAdderMarker()
    const marker = adder.marker

    // hide the marker by default
    marker.visible = false

    // function to find candidate point for all mousemove event
    const f = (
        roofs: RoofPlate[],
        evt: SceneMouseMoveEvent
    ): CandidatePoint | null => {
        if (couldBeRoof(houseWrapper, roofs, evt)) {
            return {
                position: evt.point,
                faceNormal: evt.face.normal
            }
        }

        return null
    }

    const point = snapshot(f, roofs, mouseMove)

    const showMarker = (p: CandidatePoint | null) => {
        const parent = marker.parent
        if (p == null || parent == null) {
            marker.visible = false
        } else {
            marker.visible = true

            // get the local position of the candidate point and move it
            // along the normal vector a bit. Then used as the new position
            // of the marker
            const rp = p.position.clone()
            rp.addScaledVector(p.faceNormal, 0.03)
            marker.position.copy(rp)
            adder.position.copy(rp)

            // set the right direction of the marker
            const target = p.position.clone()
            target.add(p.faceNormal)
            parent.localToWorld(target)
            marker.lookAt(target)
            adder.normal.copy(p.faceNormal)
        }
    }

    const disposable = point.run(mkSink(showMarker), defScheduler())

    const mkRoof = () => {
        return newRoofPlate(adder.position, adder.normal)
    }

    return {
        marker: marker,
        addedNewRoof: map(mkRoof, marker.tapEvents),
        disposable: disposable
    }
}
