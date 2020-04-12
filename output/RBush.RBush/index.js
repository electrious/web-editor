// Generated by purs version 0.13.6
"use strict";
var $foreign = require("./foreign.js");
var Util = require("../Util/index.js");
var search = Util.ffi([ "box", "tree", "" ])("tree.search(box)");
var load = Util.fpi([ "items", "tree", "" ])("tree.load(items)");
module.exports = {
    load: load,
    search: search,
    mkRBush: $foreign.mkRBush
};