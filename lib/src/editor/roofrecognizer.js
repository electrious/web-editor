"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var core_1 = require("@most/core");
var roofplate_1 = require("../models/roofplate");
var three_1 = require("three");
var roofcheck_1 = require("../algorithm/roofcheck");
var memoizeWith_1 = __importDefault(require("ramda/es/memoizeWith"));
var always_1 = __importDefault(require("ramda/es/always"));
var mesh_1 = require("../custom/mesh");
var sink_1 = require("../sink");
var helper_1 = require("../helper");
/**
 * functions to create geometry and material for the marker for adding new roofs
 * These functions are memoized so the objects are shared.
 */
var adderMarkerMaterial = memoizeWith_1.default(always_1.default('adder-marker-mat'), function () {
    return new three_1.MeshBasicMaterial({ color: 0x2222ff });
});
var adderMarkerGeometry = memoizeWith_1.default(always_1.default('adder-marker-geo'), function () {
    return new three_1.CircleGeometry(1, 32);
});
var createAdderMarker = function () {
    var geo = adderMarkerGeometry();
    var mat = adderMarkerMaterial();
    var marker = new mesh_1.TappableMesh(geo, mat);
    marker.name = 'add-roof-marker';
    return {
        marker: marker,
        position: new three_1.Vector3(0, 0, 0),
        normal: new three_1.Vector3(0, 1, 0)
    };
};
/**
 * Create a roof recognizer object.
 * @param houseWrapper the house mesh wrapper object
 * @param roofs a stream of the current roof plates
 * @param mouseMove a stream of mouse move events in the scene
 * @returns RoofRecognizer
 */
function createRoofRecognizer(houseWrapper, roofs, mouseMove, canShow) {
    // create the adder marker and add it to parent
    var adder = createAdderMarker();
    var marker = adder.marker;
    // hide the marker by default
    marker.visible = false;
    // function to find candidate point for all mousemove event
    var f = function (roofs, evt) {
        if (roofcheck_1.couldBeRoof(houseWrapper, roofs, evt)) {
            return {
                position: evt.point,
                faceNormal: evt.face.normal
            };
        }
        return null;
    };
    var point = core_1.snapshot(f, roofs, mouseMove);
    var showMarker = function (p) {
        var parent = marker.parent;
        if (p == null || parent == null) {
            marker.visible = false;
        }
        else {
            marker.visible = true;
            // get the local position of the candidate point and move it
            // along the normal vector a bit. Then used as the new position
            // of the marker
            var rp = p.position.clone();
            rp.addScaledVector(p.faceNormal, 0.03);
            marker.position.copy(rp);
            adder.position.copy(rp);
            // set the right direction of the marker
            var target = p.position.clone();
            target.add(p.faceNormal);
            parent.localToWorld(target);
            marker.lookAt(target);
            adder.normal.copy(p.faceNormal);
        }
    };
    var g = function (p, canShow) {
        return canShow ? p : null;
    };
    var disposable = core_1.combine(g, point, canShow).run(sink_1.mkSink(showMarker), helper_1.defScheduler());
    var mkRoof = function () {
        return roofplate_1.newRoofPlate(adder.position, adder.normal);
    };
    return {
        marker: marker,
        addedNewRoof: core_1.map(mkRoof, marker.tapEvents),
        disposable: disposable
    };
}
exports.createRoofRecognizer = createRoofRecognizer;
//# sourceMappingURL=roofrecognizer.js.map