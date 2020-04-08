// Generated by purs version 0.13.6
"use strict";
var $foreign = require("./foreign.js");
var Util = require("../Util/index.js");
var worldToLocal = Util.ffi([ "v", "o", "" ])("o.worldToLocal(v.clone())");
var userData = Util.ffi([ "o" ])("o.userData");
var updateMatrixWorld = Util.fpi([ "o", "" ])("o.updateMatrixWorld()");
var updateMatrix = Util.fpi([ "o", "" ])("o.updateMatrix()");
var translateZ = Util.fpi([ "z", "o", "" ])("o.translateZ(z)");
var translateY = Util.fpi([ "y", "o", "" ])("o.translateY(y)");
var translateX = Util.fpi([ "x", "o", "" ])("o.translateX(x)");
var setVisible = Util.fpi([ "v", "o", "" ])("o.visible = v");
var setUserData = Util.fpi([ "d", "o", "" ])("o.userData = d");
var setRenderOrder = Util.fpi([ "r", "o", "" ])("o.renderOrder = r");
var setReceiveShadow = Util.fpi([ "s", "o", "" ])("o.receiveShadow = s");
var setPosition = Util.fpi([ "v", "o", "" ])("o.position.copy(v)");
var setName = Util.fpi([ "name", "obj", "" ])("obj.name = name");
var setCastShadow = Util.fpi([ "s", "o", "" ])("o.castShadow = s");
var rotateZ = Util.fpi([ "r", "o", "" ])("o.rotateZ(r)");
var rotateY = Util.fpi([ "r", "o", "" ])("o.rotateY(r)");
var rotateX = Util.fpi([ "r", "o", "" ])("o.rotateX(r)");
var rotateOnWorldAxis = Util.fpi([ "v", "d", "o", "" ])("o.rotateOnWorldAxis(v, d)");
var remove = Util.fpi([ "child", "parent", "" ])("parent.remove(child)");
var position = Util.ffi([ "o" ])("o.position");
var parent = Util.ffi([ "o" ])("o.parent");
var matrix = Util.ffi([ "o" ])("o.matrix");
var lookAt = Util.fpi([ "v", "o", "" ])("o.lookAt(v)");
var localToWorld = Util.ffi([ "v", "o", "" ])("o.localToWorld(v.clone())");
var hasParent = Util.ffi([ "o" ])("o.parent !== null && o.parent !== undefined");
var clone = Util.ffi([ "o", "" ])("o.clone()");
var children = Util.ffi([ "o" ])("o.children");
var castShadow = Util.ffi([ "o" ])("o.castShadow");
var add = Util.fpi([ "child", "parent", "" ])("parent.add(child)");
module.exports = {
    castShadow: castShadow,
    setCastShadow: setCastShadow,
    setReceiveShadow: setReceiveShadow,
    children: children,
    hasParent: hasParent,
    parent: parent,
    add: add,
    remove: remove,
    setName: setName,
    position: position,
    setPosition: setPosition,
    rotateX: rotateX,
    rotateY: rotateY,
    rotateZ: rotateZ,
    rotateOnWorldAxis: rotateOnWorldAxis,
    translateX: translateX,
    translateY: translateY,
    translateZ: translateZ,
    setRenderOrder: setRenderOrder,
    setVisible: setVisible,
    matrix: matrix,
    updateMatrix: updateMatrix,
    updateMatrixWorld: updateMatrixWorld,
    localToWorld: localToWorld,
    worldToLocal: worldToLocal,
    lookAt: lookAt,
    clone: clone,
    userData: userData,
    setUserData: setUserData,
    mkObject3D: $foreign.mkObject3D
};
