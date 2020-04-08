// Generated by purs version 0.13.6
"use strict";
var Util = require("../Util/index.js");
var setXYZ = Util.fpi([ "idx", "x", "y", "z", "attr", "" ])("attr.setXYZ(idx, x, y, z)");
var setNeedsUpdate = Util.fpi([ "u", "attr", "" ])("attr.needsUpdate = u");
var mkShapeGeometry = Util.ffi([ "shp", "" ])("new THREE.ShapeGeometry(shp)");
var mkShape = Util.ffi([ "ps", "" ])("new THREE.Shape(ps)");
var mkCircleGeometry = Util.ffi([ "radius", "segs", "" ])("new THREE.CircleGeometry(radius, segs)");
var isBufferGeometry = Util.ffi([ "geo" ])("geo instanceof THREE.BufferGeometry");
var isBufferAttribute = Util.ffi([ "attr" ])("attr instanceof THREE.BufferAttribute");
var getZ = Util.ffi([ "idx", "attr" ])("attr.getX(idx)");
var getY = Util.ffi([ "idx", "attr" ])("attr.getX(idx)");
var getX = Util.ffi([ "idx", "attr" ])("attr.getX(idx)");
var getAttribute = Util.ffi([ "name", "geo" ])("geo.getAttribute(name)");
var count = Util.ffi([ "attr" ])("attr.count");
var clone = Util.ffi([ "geo", "" ])("geo.clone()");
module.exports = {
    clone: clone,
    mkCircleGeometry: mkCircleGeometry,
    mkShape: mkShape,
    mkShapeGeometry: mkShapeGeometry,
    isBufferGeometry: isBufferGeometry,
    getAttribute: getAttribute,
    isBufferAttribute: isBufferAttribute,
    setXYZ: setXYZ,
    setNeedsUpdate: setNeedsUpdate,
    count: count,
    getX: getX,
    getY: getY,
    getZ: getZ
};
