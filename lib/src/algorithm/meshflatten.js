"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var three_1 = require("three");
var roofplate_1 = require("../models/roofplate");
var rbush_1 = __importDefault(require("rbush"));
var pointinpolygon_1 = require("./pointinpolygon");
var flatten_1 = __importDefault(require("ramda/es/flatten"));
var angle_1 = require("../math/angle");
// offset used to calculate bounding box for a point
var vertexOffset = 0.0001;
/**
 * calculate VertexItem for a vertex point
 * @param point
 */
function vertexItem(point, normal, index) {
    var x = point.x;
    var y = point.y;
    return {
        minX: x - vertexOffset,
        maxX: x + vertexOffset,
        minY: y - vertexOffset,
        maxY: y + vertexOffset,
        vertex: point,
        normal: normal,
        index: index
    };
}
/**
 * Build an RTree from a list of vertices
 * @param vertices
 */
function buildRTree(vertices, normals) {
    var tree = new rbush_1.default();
    var items = [];
    for (var i = 0; i < vertices.length; i++) {
        var v = vertices[i];
        var n = normals[i];
        items.push(vertexItem(v, n, i));
    }
    tree.load(items);
    return tree;
}
exports.buildRTree = buildRTree;
/**
 * get the bounding box of a polygon
 * @param p
 */
function polygonBoundingBox(polygon) {
    var minX = Infinity;
    var minY = Infinity;
    var maxX = -Infinity;
    var maxY = -Infinity;
    for (var _i = 0, polygon_1 = polygon; _i < polygon_1.length; _i++) {
        var v = polygon_1[_i];
        if (v.x < minX)
            minX = v.x;
        if (v.x > maxX)
            maxX = v.x;
        if (v.y < minY)
            minY = v.y;
        if (v.y > maxY)
            maxY = v.y;
    }
    return { minX: minX, minY: minY, maxX: maxX, maxY: maxY };
}
/**
 * Internal helper class to do flattening on a roof
 */
var RoofFlattener = /** @class */ (function () {
    function RoofFlattener(normal, center, polygon) {
        this.roofNormal = normal;
        this.roofCenter = center;
        this.roofPolygon = polygon;
    }
    /**
     * flatten a vertex, returns a new position for that vertex
     * @param v
     */
    RoofFlattener.prototype.flatten = function (v) {
        var nv = this.roofCenter.clone();
        nv.addScaledVector(v, -1);
        var scale = this.roofNormal.dot(nv);
        var r = v.clone();
        r.addScaledVector(this.roofNormal, scale);
        return r;
    };
    /**
     * calculate distance from the param position to the roof.
     * @param v
     */
    RoofFlattener.prototype.distToRoof = function (v) {
        var nv = v.clone();
        nv.sub(this.roofCenter);
        return this.roofNormal.dot(nv);
    };
    return RoofFlattener;
}());
/**
 * get the RoofFlattener for a roof
 * @param roof
 */
function roofFlattener(roof) {
    var poly = roofplate_1.getRoofPolygon(roof);
    return new RoofFlattener(roof.normal, roof.center, poly);
}
/**
 * apply the flattened vertices to the BufferGeometry and return a new one
 * @param geo
 * @param fvs
 */
function applyFlattendVertex(geo, fvs) {
    var newGeo = geo.clone();
    var attr = newGeo.getAttribute('position');
    if (attr instanceof three_1.BufferAttribute) {
        for (var _i = 0, fvs_1 = fvs; _i < fvs_1.length; _i++) {
            var fv = fvs_1[_i];
            var p = fv.newPos;
            attr.setXYZ(fv.index, p.x, p.y, p.z);
        }
        attr.needsUpdate = true;
        return newGeo;
    }
    return geo;
}
/**
 * Flatten a single roof plate
 * @param tree
 * @param roof
 */
function flattenRoofplate(tree, roof) {
    var flattener = roofFlattener(roof);
    var poly = flattener.roofPolygon;
    // search for all vertices under the roof polygon bounding box
    var candidates = tree.search(polygonBoundingBox(poly));
    // for all candidates, check if it's inside the polygon.
    // if it is, then flatten that vertex
    var result = [];
    for (var _i = 0, candidates_1 = candidates; _i < candidates_1.length; _i++) {
        var cand = candidates_1[_i];
        var vec2 = new three_1.Vector2(cand.vertex.x, cand.vertex.y);
        if (pointinpolygon_1.pointInPolygon(poly, vec2)) {
            // now, the point is inside the roof polygon. check its distance
            // to the roof and the angle between its normal vector with the
            // roof normal vector.
            var angle = angle_1.Angle.fromRad(Math.acos(flattener.roofNormal.dot(cand.normal)));
            var dist = flattener.distToRoof(cand.vertex);
            // only add points that're close to the roofplate and within
            // an angle limit to flattening result.
            if ((dist < 0.5 && dist >= 0) ||
                (dist < 0 && dist > -1 && angle.deg < 20)) {
                var newPos = flattener.flatten(cand.vertex);
                result.push({ index: cand.index, newPos: newPos });
            }
        }
    }
    return result;
}
/**
 * Flatten all roofplates
 * @param geo original BufferGeometry of the house mesh
 * @param tree an RTree with all vertices of the house mesh
 * @param house the House Mesh
 * @param roofs all roofplates and their Object3D node in the scene graph
 */
function flattenRoofPlates(geo, tree, house, roofs) {
    var fvs = flatten_1.default(roofs.map(function (r) { return flattenRoofplate(tree, r); }));
    var newGeo = applyFlattendVertex(geo, fvs);
    // apply the new geometry
    house.geometry = newGeo;
}
exports.flattenRoofPlates = flattenRoofPlates;
//# sourceMappingURL=meshflatten.js.map