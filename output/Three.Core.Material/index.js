"use strict";
var $foreign = require("./foreign.js");
var Util = require("../Util/index.js");
var setTransparent = Util.fpi([ "t", "mat", "" ])("mat.transparent = t");
var setOpacity = Util.fpi([ "o", "mat", "" ])("mat.opacity = o");
var preload = Util.fpi([ "creator", "" ])("creator.preload()");
var getMaterial = Util.ffi([ "mat", "creator" ])("creator.materials[mat]");
module.exports = {
    setTransparent: setTransparent,
    setOpacity: setOpacity,
    getMaterial: getMaterial,
    preload: preload,
    mkMeshBasicMaterial: $foreign.mkMeshBasicMaterial
};
