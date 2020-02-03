"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var three_1 = require("three");
var angle_1 = require("../math/angle");
var map_1 = __importDefault(require("ramda/es/map"));
var v4_1 = __importDefault(require("uuid/v4"));
var ramda_1 = require("ramda");
var Orientation;
(function (Orientation) {
    Orientation[Orientation["Landscape"] = 0] = "Landscape";
    Orientation[Orientation["Portrait"] = 1] = "Portrait";
})(Orientation = exports.Orientation || (exports.Orientation = {}));
var Alignment;
(function (Alignment) {
    Alignment[Alignment["Grid"] = 0] = "Grid";
    Alignment[Alignment["Brick"] = 1] = "Brick";
})(Alignment = exports.Alignment || (exports.Alignment = {}));
function cloneRoof(roof) {
    return {
        id: roof.id,
        intId: roof.intId,
        leadId: roof.leadId,
        borderPoints: map_1.default(function (v) { return v.clone(); }, roof.borderPoints),
        coefs: roof.coefs,
        center: roof.center.clone(),
        normal: roof.normal.clone(),
        orientation: roof.orientation,
        alignment: roof.alignment,
        slope: roof.slope,
        azimuth: roof.azimuth,
        rotation: roof.rotation
    };
}
exports.cloneRoof = cloneRoof;
var mkVec = function (ns) {
    return new three_1.Vector3(ns[0], ns[1], ns[2]);
};
/**
 * convert an external JSRoofPlate object to internal RoofPlate model
 * @param r
 */
function fromJSRoofPlate(r) {
    return {
        id: r.uuid,
        intId: r.id,
        leadId: r.lead_id,
        borderPoints: r.border_points.map(function (v) {
            return new three_1.Vector3(v.x, v.y, v.z);
        }),
        coefs: r.coefs,
        center: mkVec(r.center),
        normal: mkVec(r.normal),
        orientation: r.orientation,
        alignment: r.alignment,
        slope: new angle_1.Angle(r.slope),
        azimuth: new angle_1.Angle(r.azimuth),
        rotation: new angle_1.Angle(r.rotation_override)
    };
}
exports.fromJSRoofPlate = fromJSRoofPlate;
/**
 * get the 2D polygon for a roof plate
 * @param roof
 */
function getRoofPolygon(roof) {
    return roof.borderPoints.map(function (v) { return new three_1.Vector2(v.x, v.y); });
}
exports.getRoofPolygon = getRoofPolygon;
/**
 * helper function to calculate angle between two Vector3
 * @param v1
 * @param v2
 */
function angleBetween(v1, v2) {
    var d = v1.dot(v2);
    var angle = Math.acos(d / (v1.length() * v2.length()));
    return angle_1.Angle.fromRad(angle);
}
/**
 * calculate the gutter vector based on roof normal vector
 * @param normal
 */
function gutterVector(normal) {
    // the gutter vector always has 0 for the z element, and it should be
    // perpendicular to the normal vector.
    // Assume normal vector to be (nx, ny, nz)
    // and let the gutter vector to be (x, y, 0).
    // First, the dot product should be 0, so 'nx * x + ny * y + nz * 0 = 0'
    // Second, let it be normalized vector, so 'x * x + y * y + 0 = 1'
    // solve the two equations and get the x,y (we only need one solution, and
    // we'll take the positive x here.)
    var nx = normal.x;
    var ny = normal.y;
    var c = nx / ny;
    var x = Math.sqrt(1 / (1 + c * c));
    var y = -x * c;
    return new three_1.Vector3(x, y, 0);
}
/**
 * get the rafter vector based on normal vector and gutter vector.
 * @param normal
 * @param gutter
 */
function rafterVector(normal, gutter) {
    var r = new three_1.Vector3(0, 0, 0);
    r.crossVectors(normal, gutter);
    return r;
}
function defBorderPoints(center, gutter, rafter) {
    var m1 = center.clone();
    m1.addScaledVector(gutter, 2);
    var m2 = center.clone();
    m2.addScaledVector(gutter, -2);
    var p1 = m1.clone();
    p1.addScaledVector(rafter, 2);
    var p2 = m1.clone();
    p2.addScaledVector(rafter, -2);
    var p3 = m2.clone();
    p3.addScaledVector(rafter, -2);
    var p4 = m2.clone();
    p4.addScaledVector(rafter, 2);
    return [p1, p2, p3, p4, p1];
}
/**
 * calculate the azimuth angle based on roof normal
 * @param normal
 */
function getAzimuth(normal) {
    var a = angle_1.Angle.fromRad(Math.atan2(normal.x, normal.y));
    if (a.deg > 360) {
        return new angle_1.Angle(a.deg - 360);
    }
    else if (a.deg < 0) {
        return new angle_1.Angle(a.deg + 360);
    }
    else
        return a;
}
/**
 * create a new RoofPlate based on the start position and normal vector.
 * @param center
 * @param normal
 */
function newRoofPlate(center, normal) {
    // normal vector projection on ground
    var projN = new three_1.Vector3(normal.x, normal.y, 0);
    var angle = angleBetween(normal, projN);
    var slope = new angle_1.Angle(90 - angle.deg);
    var azimuth = getAzimuth(normal);
    // calculate the gutter and rafter vectors and add default border points
    var gutter = gutterVector(normal);
    var rafter = rafterVector(normal, gutter);
    var borderPoints = defBorderPoints(center, gutter, rafter);
    return {
        id: v4_1.default(),
        intId: 0,
        leadId: 0,
        borderPoints: borderPoints,
        coefs: [],
        center: center.clone(),
        normal: normal.clone(),
        orientation: Orientation.Landscape,
        alignment: Alignment.Brick,
        slope: slope,
        azimuth: azimuth,
        rotation: new angle_1.Angle(0)
    };
}
exports.newRoofPlate = newRoofPlate;
/**
 * enum that defines the types of operation applied to roofs
 */
var RoofOperationType;
(function (RoofOperationType) {
    RoofOperationType[RoofOperationType["Create"] = 0] = "Create";
    RoofOperationType[RoofOperationType["Delete"] = 1] = "Delete";
    RoofOperationType[RoofOperationType["Update"] = 2] = "Update";
})(RoofOperationType = exports.RoofOperationType || (exports.RoofOperationType = {}));
function mkCreateRoofOp(roof) {
    return {
        type: RoofOperationType.Create,
        roof: roof
    };
}
exports.mkCreateRoofOp = mkCreateRoofOp;
function mkDeleteRoofOp(roof) {
    return {
        type: RoofOperationType.Delete,
        roof: roof.id
    };
}
exports.mkDeleteRoofOp = mkDeleteRoofOp;
function mkUpdateRoofOp(roof) {
    return {
        type: RoofOperationType.Update,
        roof: roof
    };
}
exports.mkUpdateRoofOp = mkUpdateRoofOp;
function pointFromVector(v) {
    return { x: v.x, y: v.y, z: v.z };
}
function toRoofEdited(roof) {
    var normal = roof.normal;
    var gutter = gutterVector(normal);
    var rafter = rafterVector(normal, gutter);
    return {
        ground: pointFromVector(gutter),
        inclined: pointFromVector(rafter),
        contours: roof.borderPoints.map(pointFromVector),
        indices: ramda_1.range(1, roof.borderPoints.length + 1)
    };
}
// exported funtion to convert a Roofplate to the RoofEdited object
function toRoofsEdited(roofs) {
    return roofs.map(toRoofEdited);
}
exports.toRoofsEdited = toRoofsEdited;
//# sourceMappingURL=roofplate.js.map