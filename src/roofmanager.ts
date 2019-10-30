import { RoofPlate } from './models/roofplate'
import { Object3D } from 'three'
import { createAdapter } from '@most/adapter'
import { map, constant, multicast, mergeArray } from '@most/core'
import equals from 'ramda/es/equals'
import { createRoofNode } from './roofnode'
import { mkSink } from './sink'
import { defScheduler } from './helper'
import { Disposable, Stream } from '@most/types'
import { disposeBoth, disposeAll } from '@most/disposable'
import pluck from 'ramda/es/pluck'

export interface RoofManager {
    roofWrapper: Object3D
    newRoof: Stream<RoofPlate>
    disposable: Disposable
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
    const nodes = roofs.map(r => {
        // create a stream for this roof noting if it's active
        const isActive = multicast(map(equals(r.id), activeRoof))
        const n = createRoofNode(r, isActive)

        // convert the tap event on this roof to the roof id
        // and pipe it into the activeRoof stream.
        const d = constant(r.id, n.tapped).run(
            mkSink(updateActive),
            defScheduler()
        )

        wrapper.add(n.roofObject)

        n.disposable = disposeBoth(n.disposable, d)

        return n
    })

    return {
        roofWrapper: wrapper,
        newRoof: mergeArray(pluck('roof', nodes)),
        disposable: disposeAll(pluck('disposable', nodes))
    }
}
