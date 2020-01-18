"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var editor_1 = require("../src/editor");
var testroofplates_1 = require("./testroofplates");
var three_1 = require("three");
var angle_1 = require("../src/math/angle");
var parent = document.querySelector('#editor');
if (parent != null) {
    // create editor instance
    var editor = editor_1.createEditor(800, 600, parent);
    var mkVec_1 = function (ns) {
        return new three_1.Vector3(ns[0], ns[1], ns[2]);
    };
    // convert test data to RoofPlate objects
    var roofs = testroofplates_1.testRoofs.roofplates.map(function (r) {
        return {
            id: r.uuid,
            intId: r.id,
            leadId: r.lead_id,
            borderPoints: r.border_points.map(function (v) {
                return new three_1.Vector3(v.x, v.y, v.z);
            }),
            coefs: r.coefs,
            center: mkVec_1(r.center),
            normal: mkVec_1(r.normal),
            orientation: r.orientation,
            alignment: r.alignment,
            slope: new angle_1.Angle(r.slope),
            azimuth: new angle_1.Angle(r.azimuth),
            rotation: new angle_1.Angle(r.rotation_override)
        };
    });
    // load the house and roofs
    editor.loadHouse(296285, roofs, function (r) {
        // updated roof
        console.log(r);
    });
}
//# sourceMappingURL=index.js.map