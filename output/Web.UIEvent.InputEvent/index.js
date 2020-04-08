// Generated by purs version 0.13.6
"use strict";
var $foreign = require("./foreign.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Web_Internal_FFI = require("../Web.Internal.FFI/index.js");
var toUIEvent = Unsafe_Coerce.unsafeCoerce;
var toEvent = Unsafe_Coerce.unsafeCoerce;
var fromUIEvent = Web_Internal_FFI.unsafeReadProtoTagged("InputEvent");
var fromEvent = Web_Internal_FFI.unsafeReadProtoTagged("InputEvent");
module.exports = {
    fromUIEvent: fromUIEvent,
    fromEvent: fromEvent,
    toUIEvent: toUIEvent,
    toEvent: toEvent,
    data_: $foreign.data_,
    isComposing: $foreign.isComposing
};
