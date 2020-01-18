"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var three_1 = require("three");
var roofplate_1 = require("../models/roofplate");
var any_1 = __importDefault(require("ramda/es/any"));
var pointinpolygon_1 = require("./pointinpolygon");
var angle_1 = require("../math/angle");
/**
 * check if there can be a roof at the point under mouse
 * @param house
 * @param roofs
 * @param e
 */
function couldBeRoof(house, roofs, e) {
    var roofPoly = roofs.map(roofplate_1.getRoofPolygon);
    // get the local coordinate of the intersection point
    // in the house mesh.
    var localPoint = house.worldToLocal(e.point);
    // 2D projection of the intersection point
    var flatP = new three_1.Vector2(localPoint.x, localPoint.y);
    // check if the point is under any roof
    var underRoof = any_1.default(function (poly) { return pointinpolygon_1.pointInPolygon(poly, flatP); }, roofPoly);
    if (underRoof)
        return false;
    // check if the intersected face has a reasonable slope
    var norm = e.face.normal;
    var up = new three_1.Vector3(0, 0, 1);
    var angle = angle_1.Angle.fromRad(Math.acos(up.dot(norm)));
    return angle.deg < 60;
}
exports.couldBeRoof = couldBeRoof;
//# sourceMappingURL=roofcheck.js.map