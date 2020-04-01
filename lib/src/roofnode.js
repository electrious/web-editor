"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var roofplate_1 = require("./models/roofplate");
var three_1 = require("three");
var mesh_1 = require("./custom/mesh");
var memoizeWith_1 = __importDefault(require("ramda/es/memoizeWith"));
var always_1 = __importDefault(require("ramda/es/always"));
var sink_1 = require("./sink");
var helper_1 = require("./helper");
var roofeditor_1 = require("./editor/roofeditor");
var disposable_1 = require("@most/disposable");
var core_1 = require("@most/core");
var adapter_1 = require("@most/adapter");
var curry_1 = __importDefault(require("ramda/es/curry"));
var compose_1 = __importDefault(require("ramda/es/compose"));
var map_1 = __importDefault(require("ramda/es/map"));
var init_1 = __importDefault(require("ramda/es/init"));
var append_1 = __importDefault(require("ramda/es/append"));
var head_1 = __importDefault(require("ramda/es/head"));
var shamos_hoey_1 = __importDefault(require("shamos-hoey"));
/**
 * Get the default material for roofplate. This function is memoized so the
 * actual material is created only once and shared.
 */
var getDefMaterial = memoizeWith_1.default(always_1.default('def_material'), function () {
    var mat = new three_1.MeshBasicMaterial({ color: 0xffffbb });
    mat.transparent = true;
    mat.opacity = 0.7;
    return mat;
});
/**
 * Get the material for an active roofpalte. This function is memoized so the
 * actual material is created only once and shared.
 */
var getActiveMaterial = memoizeWith_1.default(always_1.default('active_material'), function () {
    var mat = new three_1.MeshBasicMaterial({ color: 0xffff88 });
    mat.transparent = true;
    mat.opacity = 0.9;
    return mat;
});
// create roof mesh
function createRoofMesh(ps, active) {
    // create a ShapeGeometry with the Shape from border points
    var shp = new three_1.Shape(ps);
    var mat = active ? getActiveMaterial() : getDefMaterial();
    return new mesh_1.TappableMesh(new three_1.ShapeGeometry(shp), mat);
}
var updateRoofPlate = curry_1.default(function (roof, ps) {
    var newRoof = roofplate_1.cloneRoof(roof);
    // make sure the first and last point are the same
    var headEl = head_1.default(ps);
    if (headEl == undefined)
        return newRoof;
    newRoof.borderPoints = append_1.default(headEl, ps);
    return newRoof;
});
// test if a polygon is simple or with self-intersections
function testSimplePolygon(ps) {
    var psArr = ps.map(function (p) {
        return [p.x, p.y];
    });
    return shamos_hoey_1.default({ type: 'Polygon', coordinates: [psArr] });
}
/**
 * create RoofNode for a RoofPlate
 * @param roof
 */
function createRoofNode(roof, isActive) {
    var obj = new three_1.Object3D();
    obj.name = 'roofplate';
    // set the roof node position
    var c = roof.center;
    obj.position.set(c.x, c.y, c.z + 0.05);
    // rotate the roof node to the right azimuth and slope angles
    obj.rotateZ(-roof.azimuth.rad);
    obj.rotateX(-roof.slope.rad);
    // make sure the matrix and matrixWorld are updated immediately after
    // position and rotation changed, so that worldToLocal can use them
    // to convert coordinates correctly
    obj.updateMatrix();
    obj.updateMatrixWorld();
    // convert all roof border points to local coordinate
    // and get only the x,y coordinates
    // NOTE: the last point will be dropped here because it's the same with the
    // first one.
    var ps = init_1.default(map_1.default(function (p) {
        var np = p.clone();
        obj.worldToLocal(np);
        return new three_1.Vector2(np.x, np.y);
    }, roof.borderPoints));
    var scheduler = helper_1.defScheduler();
    // add the roof mesh
    var mesh = createRoofMesh(ps);
    obj.add(mesh);
    // update mesh material when the roof is activated/deactivated
    var disposable1 = isActive.run(sink_1.mkSink(function (active) {
        mesh.material = active ? getActiveMaterial() : getDefMaterial();
    }), scheduler);
    // create the vertex markers
    var editor = roofeditor_1.createRoofEditor(obj, isActive, ps);
    // create a stream for tap event of the mesh
    var _a = adapter_1.createAdapter(), updateTap = _a[0], tapped = _a[1];
    // pipe mesh tap events into the new tapped stream
    var updateTapDispose = mesh.tapEvents.run(sink_1.mkSink(updateTap), scheduler);
    // update roof corner positions and the mesh
    var updatePos = function (arr) {
        // remove old mesh and dispose old tap event pipe
        obj.remove(mesh);
        disposable_1.dispose(updateTapDispose);
        // create new mesh and setup the tap event pipe
        mesh = createRoofMesh(arr, true);
        obj.add(mesh);
        updateTapDispose = mesh.tapEvents.run(sink_1.mkSink(updateTap), scheduler);
    };
    var disposable2 = editor.roofVertices.run(sink_1.mkSink(updatePos), scheduler);
    var toParent = function (v) {
        // convert the local position to parent's coordinate
        return new three_1.Vector3(v.x, v.y, 0).applyMatrix4(obj.matrix);
    };
    var newRoofs = core_1.map(compose_1.default(roofplate_1.mkUpdateRoofOp, updateRoofPlate(roof), map_1.default(toParent)), editor.roofVertices);
    // create a stream for delete event if the current roof is not simple.
    var _b = adapter_1.createAdapter(), toDel = _b[0], delEvt = _b[1];
    if (!testSimplePolygon(ps)) {
        setTimeout(function () {
            toDel();
        }, 1000);
    }
    var delRoofEvt = core_1.merge(delEvt, editor.deleteRoof);
    return {
        roofId: roof.id,
        roofDelete: core_1.constant(roofplate_1.mkDeleteRoofOp(roof), delRoofEvt),
        roofUpdate: core_1.multicast(newRoofs),
        tapped: tapped,
        roofObject: obj,
        disposable: disposable_1.disposeAll([
            editor.disposable,
            disposable1,
            disposable2,
            updateTapDispose
        ])
    };
}
exports.createRoofNode = createRoofNode;
//# sourceMappingURL=roofnode.js.map