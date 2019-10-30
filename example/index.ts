import { createEditor } from '../src/editor'
import { testRoofs } from './testroofplates'
import { Vector3 } from 'three'
import { RoofPlate } from '../src/models/roofplate'
import { Angle } from '../src/math/angle'
import { mkSink } from '../src/sink'
import { defScheduler, debug } from '../src/helper'

const parent = document.querySelector('#editor')

if (parent != null) {
    // create editor instance
    const editor = createEditor(800, 600, parent)

    const mkVec = (ns: number[]) => {
        return new Vector3(ns[0], ns[1], ns[2])
    }

    // convert test data to RoofPlate objects
    const roofs = testRoofs.roofplates.map(
        (r): RoofPlate => {
            return {
                id: r.uuid,
                intId: r.id,
                leadId: r.lead_id,
                borderPoints: r.border_points.map(v => {
                    return new Vector3(v.x, v.y, v.z)
                }),
                coefs: r.coefs,
                center: mkVec(r.center),
                normal: mkVec(r.normal),
                orientation: r.orientation,
                alignment: r.alignment,
                slope: new Angle(r.slope),
                azimuth: new Angle(r.azimuth),
                rotation: new Angle(r.rotation_override)
            }
        }
    )

    // load the house and roofs
    const newRoofs = debug(editor.loadHouse(296285, roofs))
    newRoofs.run(mkSink(), defScheduler())
}
