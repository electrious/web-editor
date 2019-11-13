import { RoofPlate } from './models/roofplate'
import { Object3D } from 'three'
import { createAdapter } from '@most/adapter'
import { constant, multicast, mergeArray, map, scan } from '@most/core'
import equals from 'ramda/es/equals'
import { createRoofNode, RoofNode } from './roofnode'
import { mkSink } from './sink'
import { defScheduler } from './helper'
import { Disposable, Stream } from '@most/types'
import { disposeBoth, disposeAll } from '@most/disposable'
import pluck from 'ramda/es/pluck'
import { HouseMeshData } from './house'
import { flattenRoofPlates } from './algorithm/meshflatten'
import values from 'ramda/es/values'
import curry from 'ramda/es/curry'

export interface RoofManager {
    roofWrapper: Object3D
    newRoof: Stream<RoofPlate>
    disposable: Disposable
}

type RoofDict = { [key: string]: RoofPlate }

function roofDict(roofs: RoofPlate[]) {
    const dict: RoofDict = {}
    for (const r of roofs) {
        dict[r.id] = r
    }

    return dict
}

type RoofObjectDict = { [key: string]: Object3D }

function roofObjDict(nodes: RoofNode[]) {
    const dict: RoofObjectDict = {}
    for (const n of nodes) {
        dict[n.roofId] = n.roofObject
    }
    return dict
}

function roofParams(
    roofD: RoofDict,
    nodeD: RoofObjectDict
): [RoofPlate, Object3D][] {
    const roofs = values(roofD)
    const f = (r: RoofPlate): [RoofPlate, Object3D] => {
        const n = nodeD[r.id]
        return [r, n]
    }
    return roofs.map(f)
}

const doFlatten = curry(
    (meshData: HouseMeshData, objDict: RoofObjectDict, rd: RoofDict) => {
        flattenRoofPlates(
            meshData.geometry,
            meshData.verticeTree,
            meshData.mesh,
            roofParams(rd, objDict)
        )
    }
)
/**
 * create RoofManager for an array of roofs
 * @param roofs
 */
export function createRoofManager(
    meshData: HouseMeshData,
    roofs: RoofPlate[]
): RoofManager {
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
    const objDict = roofObjDict(nodes)

    const newRoof = multicast(mergeArray(pluck('roof', nodes)))

    const f = (dict: RoofDict, roof: RoofPlate) => {
        dict[roof.id] = roof

        return dict
    }

    const defRoofDict = roofDict(roofs)
    doFlatten(meshData, objDict, defRoofDict)

    const roofDicts = scan(f, defRoofDict, newRoof)

    const d = roofDicts.run(
        mkSink(doFlatten(meshData, objDict)),
        defScheduler()
    )

    return {
        roofWrapper: wrapper,
        newRoof: newRoof,
        disposable: disposeBoth(disposeAll(pluck('disposable', nodes)), d)
    }
}
