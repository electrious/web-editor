"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var core_1 = require("@most/core");
var memoizeWith_1 = __importDefault(require("ramda/es/memoizeWith"));
var always_1 = __importDefault(require("ramda/es/always"));
var scheduler_1 = require("@most/scheduler");
/**
 * get the default scheduler used in stream processing. This function is
 * memoized so the scheduler is created once and shared.
 */
exports.defScheduler = memoizeWith_1.default(always_1.default('def_scheduler'), function () {
    return scheduler_1.newDefaultScheduler();
});
/**
 * unwrap values that can be null in a stream to non-null stream
 * @param src
 */
function unwrap(src) {
    return core_1.filter(function (v) { return v != null && v != undefined; }, src);
}
exports.unwrap = unwrap;
/**
 * add a debug log to the stream
 * @param src
 */
function debug(src) {
    return core_1.tap(function (v) { return console.log(v); }, src);
}
exports.debug = debug;
/**
 * Only propagate events in 'stream' when 'pred' is true.
 * @param pred
 * @param stream
 */
function gate(pred, stream) {
    return unwrap(core_1.snapshot(function (pred, v) { return (pred ? v : null); }, pred, stream));
}
exports.gate = gate;
//# sourceMappingURL=helper.js.map