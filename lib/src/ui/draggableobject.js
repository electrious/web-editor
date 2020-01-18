"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var three_1 = require("three");
var core_1 = require("@most/core");
var memoizeWith_1 = __importDefault(require("ramda/es/memoizeWith"));
var always_1 = __importDefault(require("ramda/es/always"));
var mesh_1 = require("../custom/mesh");
var sink_1 = require("../sink");
var helper_1 = require("../helper");
var clone_1 = __importDefault(require("ramda/es/clone"));
var sceneevent_1 = require("../sceneevent");
// get the default material. This function is memoized so the material is
// only created once and shared.
var getMaterial = memoizeWith_1.default(always_1.default('marker_material'), function () {
    return new three_1.MeshBasicMaterial({ color: 0xff2222 });
});
// create the visible part of the object, user can specify custom geometry and
// material
function createVisibleObject(customGeo, customMat) {
    var geo = customGeo == undefined ? new three_1.CircleGeometry(0.5, 32) : customGeo;
    var mat = customMat == undefined ? getMaterial() : customMat;
    return new mesh_1.TapDragMesh(geo, mat);
}
// get invisible material for the big circle under marker to ease dragging.
// this function is memoized so the material is created only once and shared.
var getInvisibleMaterial = memoizeWith_1.default(always_1.default('invisible_material'), function () {
    var mat = new three_1.MeshBasicMaterial();
    mat.transparent = true;
    mat.opacity = 0.01;
    return mat;
});
function createInvisibleCircle() {
    var geo = new three_1.CircleGeometry(10, 32);
    var mat = getInvisibleMaterial();
    return new mesh_1.DraggableMesh(geo, mat);
}
/**
 * create a draggable object.
 * @param active a stream signalling if the current roof is active or not
 * @param position 2D start position of the object
 * @param customGeo optional custom geometry for the object
 * @param customMat optional custom material for the object
 * @returns DraggableObject
 */
exports.createDraggableObject = function (active, index, position, customGeo, customMat) {
    var dragObj = new three_1.Object3D();
    dragObj.name = 'drag-object';
    // create the visible marker
    var mesh = createVisibleObject(customGeo, customMat);
    var defPosition = new three_1.Vector3(position.x, position.y, 0.1);
    mesh.position.copy(defPosition);
    mesh.visible = false;
    dragObj.add(mesh);
    // create the invisible circle
    var invCircle = createInvisibleCircle();
    invCircle.position.copy(defPosition);
    invCircle.visible = false;
    invCircle.renderOrder = 10;
    dragObj.add(invCircle);
    var sched = helper_1.defScheduler();
    var disposable = active.run(sink_1.mkSink(function (a) { return (mesh.visible = a); }), sched);
    var dragEvts = core_1.multicast(core_1.merge(mesh.dragEvents, invCircle.dragEvents));
    var evts = core_1.multicast(mesh_1.validateDrag(dragEvts));
    var startEvt = core_1.filter(sceneevent_1.isDragStart, evts);
    var endEvt = core_1.filter(sceneevent_1.isDragEnd, evts);
    var dragging = core_1.skipRepeats(core_1.merge(core_1.constant(true, startEvt), core_1.constant(false, endEvt)));
    dragging.run(sink_1.mkSink(function (d) { return (invCircle.visible = d); }), sched);
    var delta = mesh_1.calcDragDelta(evts, function (v) {
        var obj = dragObj.parent;
        return obj == null ? null : obj.worldToLocal(v);
    });
    // function to update the mesh position based on dragDelta
    var updatePos = function (lastPos, delta) {
        var np = clone_1.default(lastPos);
        var nDelta = new three_1.Vector3(delta.x, delta.y, 0);
        np.add(nDelta);
        mesh.position.copy(np);
        invCircle.position.copy(np);
        return np;
    };
    var newPos = core_1.scan(updatePos, defPosition, delta);
    return {
        object: dragObj,
        tapped: core_1.constant(index, mesh.tapEvents),
        position: core_1.multicast(newPos),
        isDragging: core_1.multicast(dragging),
        disposable: disposable
    };
};
//# sourceMappingURL=draggableobject.js.map