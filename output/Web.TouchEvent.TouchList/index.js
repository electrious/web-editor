// Generated by purs version 0.13.6
"use strict";
var $foreign = require("./foreign.js");
var Data_Nullable = require("../Data.Nullable/index.js");
var item = function (i) {
    return function (l) {
        return Data_Nullable.toMaybe($foreign["_item"](i, l));
    };
};
module.exports = {
    item: item,
    length: $foreign.length
};
