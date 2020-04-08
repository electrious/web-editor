// Generated by purs version 0.13.6
"use strict";
var $foreign = require("./foreign.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Show = require("../Data.Show/index.js");
var Util = require("../Util/index.js");
var Vector = function (add, addScaled, cross, dist, dot, length, multiplyScalar, normal, sub) {
    this.add = add;
    this.addScaled = addScaled;
    this.cross = cross;
    this.dist = dist;
    this.dot = dot;
    this.length = length;
    this.multiplyScalar = multiplyScalar;
    this.normal = normal;
    this.sub = sub;
};
var HasZ = function (vecZ) {
    this.vecZ = vecZ;
};
var HasY = function (vecY) {
    this.vecY = vecY;
};
var HasX = function (vecX) {
    this.vecX = vecX;
};
var vecZ = function (dict) {
    return dict.vecZ;
};
var vecY = function (dict) {
    return dict.vecY;
};
var vecX = function (dict) {
    return dict.vecX;
};
var vLength = Util.ffi([ "v" ])("v.length()");
var vDot = Util.ffi([ "v1", "v2" ])("v1.dot(v2)");
var vDist = Util.ffi([ "v1", "v2" ])("v1.distanceTo(v2)");
var vecVec2 = new Vector($foreign.vAdd, $foreign.vAddScaled, $foreign.vCross, vDist, vDot, vLength, $foreign.vMultiplyScalar, $foreign.vNormal, $foreign.vSub);
var vecVec3 = new Vector($foreign.vAdd, $foreign.vAddScaled, $foreign.vCross, vDist, vDot, vLength, $foreign.vMultiplyScalar, $foreign.vNormal, $foreign.vSub);
var sub = function (dict) {
    return dict.sub;
};
var showVec3 = new Data_Show.Show(Util.ffi([ "vec" ])("'(' + vec.x + ', ' + vec.y + ', ' + vec.z + ')'"));
var showVec2 = new Data_Show.Show(Util.ffi([ "vec" ])("'(' + vec.x + ', ' + vec.y + ')'"));
var normal = function (dict) {
    return dict.normal;
};
var multiplyScalar = function (dict) {
    return dict.multiplyScalar;
};
var length = function (dict) {
    return dict.length;
};
var getZ = Util.ffi([ "vec" ])("vec.z");
var hasZVec3 = new HasZ(getZ);
var getY = Util.ffi([ "vec" ])("vec.y");
var hasYVec2 = new HasY(getY);
var hasYVec3 = new HasY(getY);
var getX = Util.ffi([ "vec" ])("vec.x");
var hasXVec2 = new HasX(getX);
var hasXVec3 = new HasX(getX);
var eqVec3 = new Data_Eq.Eq($foreign.vEq);
var eqVec2 = new Data_Eq.Eq($foreign.vEq);
var dot = function (dict) {
    return dict.dot;
};
var dist = function (dict) {
    return dict.dist;
};
var cross = function (dict) {
    return dict.cross;
};
var addScaled = function (dict) {
    return dict.addScaled;
};
var add = function (dict) {
    return dict.add;
};
module.exports = {
    HasX: HasX,
    vecX: vecX,
    HasY: HasY,
    vecY: vecY,
    HasZ: HasZ,
    vecZ: vecZ,
    Vector: Vector,
    dot: dot,
    length: length,
    dist: dist,
    cross: cross,
    add: add,
    addScaled: addScaled,
    sub: sub,
    normal: normal,
    multiplyScalar: multiplyScalar,
    eqVec2: eqVec2,
    eqVec3: eqVec3,
    hasXVec2: hasXVec2,
    hasYVec2: hasYVec2,
    hasXVec3: hasXVec3,
    hasYVec3: hasYVec3,
    hasZVec3: hasZVec3,
    showVec2: showVec2,
    showVec3: showVec3,
    vecVec2: vecVec2,
    vecVec3: vecVec3,
    mkVec2: $foreign.mkVec2,
    mkVec3: $foreign.mkVec3,
    applyMatrix: $foreign.applyMatrix
};
