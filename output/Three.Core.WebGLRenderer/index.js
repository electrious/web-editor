// Generated by purs version 0.13.6
"use strict";
var $foreign = require("./foreign.js");
var Util = require("../Util/index.js");
var setSize = Util.fpi([ "w", "h", "r", "" ])("r.setSize(w, h)");
var render = Util.fpi([ "scene", "camera", "r", "" ])("r.render(scene, camera)");
var domElement = Util.ffi([ "r" ])("r.domElement");
module.exports = {
    setSize: setSize,
    domElement: domElement,
    render: render,
    mkWebGLRenderer: $foreign.mkWebGLRenderer
};