"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var roofplate_1 = require("./models/roofplate");
var three_1 = require("three");
var adapter_1 = require("@most/adapter");
var core_1 = require("@most/core");
var equals_1 = __importDefault(require("ramda/es/equals"));
var roofnode_1 = require("./roofnode");
var sink_1 = require("./sink");
var helper_1 = require("./helper");
var disposable_1 = require("@most/disposable");
var pluck_1 = __importDefault(require("ramda/es/pluck"));
var meshflatten_1 = require("./algorithm/meshflatten");
var values_1 = __importDefault(require("ramda/es/values"));
var curry_1 = __importDefault(require("ramda/es/curry"));
var map_1 = __importDefault(require("ramda/es/map"));
var roofrecognizer_1 = require("./editor/roofrecognizer");
var ramda_1 = require("ramda");
function roofDict(roofs) {
    var dict = {};
    for (var _i = 0, roofs_1 = roofs; _i < roofs_1.length; _i++) {
        var r = roofs_1[_i];
        dict[r.id] = r;
    }
    return dict;
}
// helper functions to extract fields from RoofDictData
function roofs(rd) {
    return rd.roofs;
}
function roofsToRender(rd) {
    return rd.roofsToRender;
}
/**
 * Update the managed roof dict with new operation
 * @param rd
 * @param op
 */
function updateRoofDict(rd, op) {
    if (op.type == roofplate_1.RoofOperationType.Create) {
        var r = op.roof;
        rd.roofs[r.id] = r;
        return { roofs: rd.roofs, roofsToRender: rd.roofs };
    }
    else if (op.type == roofplate_1.RoofOperationType.Delete) {
        var rid = op.roof;
        delete rd.roofs[rid];
        return { roofs: rd.roofs, roofsToRender: rd.roofs };
    }
    else {
        var r = op.roof;
        rd.roofs[r.id] = r;
        return { roofs: rd.roofs, roofsToRender: null };
    }
}
var doFlatten = curry_1.default(function (meshData, rd) {
    meshflatten_1.flattenRoofPlates(meshData.geometry, meshData.verticeTree, meshData.mesh, values_1.default(rd));
});
/**
 * get roofUpdate event stream from an array of roof nodes
 * @param ns
 */
function getRoofUpdate(ns) {
    return core_1.mergeArray(pluck_1.default('roofUpdate', ns));
}
/**
 * get roofDelete event stream from an array of roof nodes
 * @param ns
 */
function getRoofDelete(ns) {
    return core_1.mergeArray(pluck_1.default('roofDelete', ns));
}
/**
 * get the activated roof id stream from an array of roof nodes
 * @param ns
 */
function getActivated(ns) {
    return core_1.mergeArray(ns.map(function (n) {
        return core_1.constant(n.roofId, n.tapped);
    }));
}
/**
 * create RoofManager for an array of roofs
 * @param roofs
 */
function createRoofManager(meshData, defRoofs) {
    var wrapper = new three_1.Object3D();
    wrapper.name = 'roof wrapper';
    var scheduler = helper_1.defScheduler();
    // create a stream for the current active roof id
    var _a = adapter_1.createAdapter(), updateActive = _a[0], activeRoof = _a[1];
    // if house mesh is tapped, to deactivate all roofs
    var d4 = core_1.constant(null, meshData.mesh.tappedEvent).run(sink_1.mkSink(updateActive), scheduler);
    // only enable house to add new roof if no roof is currently active.
    var enableAddingRoof = function (r) {
        meshData.mesh.enableAddingRoof(r == null);
    };
    var d5 = activeRoof.run(sink_1.mkSink(enableAddingRoof), scheduler);
    var mkNode = function (r) {
        // create a stream for this roof noting if it's active
        var isActive = core_1.multicast(core_1.map(equals_1.default(r.id), activeRoof));
        return roofnode_1.createRoofNode(r, isActive);
    };
    var _b = adapter_1.createAdapter(), updateRoofsData = _b[0], roofsData = _b[1];
    var defRoofDict = roofDict(defRoofs);
    // get the roofs to be rerendered
    var rsToRender = helper_1.unwrap(core_1.map(roofsToRender, roofsData));
    var rsToRenderLst = core_1.map(values_1.default, rsToRender);
    // create roof node for each roof
    var nodes = core_1.multicast(core_1.map(map_1.default(mkNode), rsToRenderLst));
    var d = core_1.switchLatest(core_1.map(getActivated, nodes)).run(sink_1.mkSink(updateActive), scheduler);
    // helper function to delete and re-add roof nodes
    var renderNodes = function (rendered, newNodes) {
        rendered.forEach(function (n) { return wrapper.remove(n.roofObject); });
        newNodes.forEach(function (n) { return wrapper.add(n.roofObject); });
        return newNodes;
    };
    // do the renderring
    var d1 = core_1.scan(renderNodes, [], nodes).run(sink_1.mkSink(), scheduler);
    // stream of delete roof operations
    var deleteRoofOp = core_1.multicast(core_1.switchLatest(core_1.map(getRoofDelete, nodes)));
    var roofChangeOp = core_1.switchLatest(core_1.map(getRoofUpdate, nodes));
    // stream of new roofs that will be updated on any change
    // and run the roof flatten algorithm whenever there's new roof change.
    var newRoofs = core_1.multicast(core_1.map(roofs, roofsData));
    var d2 = newRoofs.run(sink_1.mkSink(doFlatten(meshData)), scheduler);
    // create the roof recognizer and add it to the roof wrapper object.
    var canShowRecognizer = helper_1.debug(core_1.map(function (a) { return a === null; }, activeRoof));
    var recognizer = roofrecognizer_1.createRoofRecognizer(meshData.wrapper, core_1.map(values_1.default, newRoofs), meshData.mesh.mouseMoveEvent, canShowRecognizer);
    wrapper.add(recognizer.marker);
    // get the add new roof operation
    var addRoofOp = core_1.multicast(core_1.map(roofplate_1.mkCreateRoofOp, recognizer.addedNewRoof));
    // merge the add roof, delete roof and any change operations
    var ops = core_1.mergeArray([addRoofOp, deleteRoofOp, roofChangeOp]);
    // manage all roofs and update it with user operations.
    var defRoofData = { roofs: defRoofDict, roofsToRender: defRoofDict };
    // apply roof operations to the managed roofs dict
    var d3 = core_1.scan(updateRoofDict, defRoofData, ops).run(sink_1.mkSink(updateRoofsData), scheduler);
    updateActive(null);
    var getRoofEdited = ramda_1.compose(roofplate_1.toRoofsEdited, values_1.default);
    return {
        roofWrapper: wrapper,
        editedRoofs: core_1.multicast(
        // skip the first elem, which is the default data.
        core_1.debounce(1000, core_1.map(getRoofEdited, core_1.skip(1, newRoofs)))),
        disposable: disposable_1.disposeAll([d, d1, d2, d3, d4, d5])
    };
}
exports.createRoofManager = createRoofManager;
//# sourceMappingURL=roofmanager.js.map