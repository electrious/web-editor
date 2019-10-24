import { RoofPlate } from './models/roofplate'
import { Object3D } from 'three'
import { createAdapter } from '@most/adapter'
import { map } from '@most/core'
import equals from 'ramda/es/equals'
import { createRoofNode } from './roofnode'
import { mkSink } from './sink'
import { defScheduler } from './helper'
import always from 'ramda/es/always'

export interface RoofManager {
    roofWrapper: Object3D
}

/**
 * create RoofManager for an array of roofs
 * @param roofs
 */
export function createRoofManager(roofs: RoofPlate[]): RoofManager {
    const wrapper = new Object3D()
    wrapper.name = 'roof wrapper'

    // create a stream for the current active roof id
    const [updateActive, activeRoof] = createAdapter<string>()

    // create roof node for each roof
    roofs.forEach(r => {
        // create a stream for this roof noting if it's active
        const isActive = map(equals(r.id), activeRoof)
        const n = createRoofNode(r, isActive)

        // convert the tap event on this roof to the roof id
        // and pipe it into the activeRoof stream.
        map(always(r.id), n.tapped).run(mkSink(updateActive), defScheduler())

        wrapper.add(n.roofObject)
    })

    return {
        roofWrapper: wrapper
    }
}
