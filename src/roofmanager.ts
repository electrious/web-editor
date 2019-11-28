import {
    RoofPlate,
    RoofOperation,
    RoofOperationType,
    mkCreateRoofOp
} from './models/roofplate'
import { Object3D } from 'three'
import { createAdapter } from '@most/adapter'
import {
    constant,
    multicast,
    mergeArray,
    map,
    scan,
    switchLatest
} from '@most/core'
import equals from 'ramda/es/equals'
import { createRoofNode, RoofNode } from './roofnode'
import { mkSink } from './sink'
import { defScheduler, unwrap, debug } from './helper'
import { Disposable, Stream } from '@most/types'
import { disposeAll } from '@most/disposable'
import pluck from 'ramda/es/pluck'
import { HouseMeshData } from './house'
import { flattenRoofPlates } from './algorithm/meshflatten'
import values from 'ramda/es/values'
import curry from 'ramda/es/curry'
import fmap from 'ramda/es/map'
import { createRoofRecognizer } from './editor/roofrecognizer'

export interface RoofManager {
    roofWrapper: Object3D
    roofOps: Stream<RoofOperation>
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

// internal data structure used to manage roofs
interface RoofDictData {
    roofs: RoofDict // all roofs managed. will be updated on any changes.
    roofsToRender: RoofDict | null // roofs to be used to rerender
}

// helper functions to extract fields from RoofDictData
function roofsForFlatten(rd: RoofDictData): RoofDict {
    return rd.roofs
}

function roofsToRender(rd: RoofDictData): RoofDict | null {
    return rd.roofsToRender
}

/**
 * Update the managed roof dict with new operation
 * @param rd
 * @param op
 */
function updateRoofDict(rd: RoofDictData, op: RoofOperation): RoofDictData {
    if (op.type == RoofOperationType.Create) {
        const r = op.roof as RoofPlate
        rd.roofs[r.id] = r

        return { roofs: rd.roofs, roofsToRender: rd.roofs }
    } else if (op.type == RoofOperationType.Delete) {
        const rid = op.roof as string
        delete rd.roofs[rid]

        return { roofs: rd.roofs, roofsToRender: rd.roofs }
    } else {
        const r = op.roof as RoofPlate
        rd.roofs[r.id] = r

        return { roofs: rd.roofs, roofsToRender: null }
    }
}

const doFlatten = curry((meshData: HouseMeshData, rd: RoofDict) => {
    flattenRoofPlates(
        meshData.geometry,
        meshData.verticeTree,
        meshData.mesh,
        values(rd)
    )
})

/**
 * get roofUpdate event stream from an array of roof nodes
 * @param ns
 */
function getRoofUpdate(ns: RoofNode[]): Stream<RoofOperation> {
    return mergeArray(pluck('roofUpdate', ns))
}

/**
 * get roofDelete event stream from an array of roof nodes
 * @param ns
 */
function getRoofDelete(ns: RoofNode[]): Stream<RoofOperation> {
    return mergeArray(pluck('roofDelete', ns))
}

/**
 * get roofForFlatten event stream from an array of roof nodes
 * @param ns
 */
function getRoofChange(ns: RoofNode[]): Stream<RoofOperation> {
    return mergeArray(pluck('roofForFlatten', ns))
}

/**
 * get the activated roof id stream from an array of roof nodes
 * @param ns
 */
function getActivated(ns: RoofNode[]): Stream<string> {
    return mergeArray(
        ns.map(n => {
            return constant(n.roofId, n.tapped)
        })
    )
}

/**
 * create RoofManager for an array of roofs
 * @param roofs
 */
export function createRoofManager(
    meshData: HouseMeshData,
    defRoofs: RoofPlate[]
): RoofManager {
    const wrapper = new Object3D()
    wrapper.name = 'roof wrapper'

    const scheduler = defScheduler()
    // create a stream for the current active roof id
    const [updateActive, activeRoof] = createAdapter<string>()

    const mkNode = (r: RoofPlate): RoofNode => {
        // create a stream for this roof noting if it's active
        const isActive = multicast(map(equals(r.id), activeRoof))
        return createRoofNode(r, isActive)
    }

    const [updateRoofsData, roofsData] = createAdapter<RoofDictData>()
    const defRoofDict = roofDict(defRoofs)

    // get the roofs to be rerendered
    const roofs = unwrap(map(roofsToRender, roofsData))
    const roofsLst = map(values, roofs)
    // create roof node for each roof
    const nodes = multicast(map(fmap(mkNode), roofsLst))

    const d = switchLatest(map(getActivated, nodes)).run(
        mkSink(updateActive),
        scheduler
    )

    // helper function to delete and re-add roof nodes
    const renderNodes = (
        rendered: RoofNode[],
        newNodes: RoofNode[]
    ): RoofNode[] => {
        rendered.forEach(n => wrapper.remove(n.roofObject))
        newNodes.forEach(n => wrapper.add(n.roofObject))
        return newNodes
    }

    // do the renderring
    const d1 = scan(renderNodes, [], nodes).run(mkSink(), scheduler)

    // stream of operation that roof updated. It only fires after user stop
    // making any new changes.
    const updatedRoofOp = multicast(switchLatest(map(getRoofUpdate, nodes)))

    // stream of delete roof operations
    const deleteRoofOp = multicast(switchLatest(map(getRoofDelete, nodes)))

    const roofChangeOp = switchLatest(map(getRoofChange, nodes))

    // stream of new roofs that will be updated on any change
    // and run the roof flatten algorithm whenever there's new roof change.
    const newRoofs = multicast(map(roofsForFlatten, roofsData))
    const d2 = newRoofs.run(mkSink(doFlatten(meshData)), scheduler)

    // create the roof recognizer and add it to the roof wrapper object.
    const recognizer = createRoofRecognizer(
        meshData.wrapper,
        map(values, newRoofs),
        meshData.mesh.mouseMoveEvent
    )

    wrapper.add(recognizer.marker)

    // get the add new roof operation
    const addRoofOp = multicast(map(mkCreateRoofOp, recognizer.addedNewRoof))

    // merge the add roof, delete roof and any change operations
    const ops = mergeArray([addRoofOp, deleteRoofOp, roofChangeOp])

    // manage all roofs and update it with user operations.
    const defRoofData = { roofs: defRoofDict, roofsToRender: defRoofDict }
    // apply roof operations to the managed roofs dict
    const d3 = scan(updateRoofDict, defRoofData, ops).run(
        mkSink(updateRoofsData),
        scheduler
    )

    return {
        roofWrapper: wrapper,
        roofOps: multicast(
            mergeArray([addRoofOp, deleteRoofOp, updatedRoofOp])
        ),
        disposable: disposeAll([d, d1, d2, d3])
    }
}
