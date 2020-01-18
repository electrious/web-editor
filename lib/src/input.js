"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var core_1 = require("@most/core");
var dom_event_1 = require("@most/dom-event");
var ramda_1 = require("ramda");
var helper_1 = require("./helper");
var not_1 = __importDefault(require("ramda/es/not"));
var adapter_1 = require("@most/adapter");
var sink_1 = require("./sink");
var disposable_1 = require("@most/disposable");
/**
 * Tap gesture recognizer for touches
 * @param elem
 */
function tapped(start, end) {
    var canBeTap = core_1.startWith(false, core_1.merge(core_1.constant(false, start), core_1.constant(true, end)));
    // the touch should end in less than 0.32 seconds to be considered a tap.
    var tapCheckEvt = core_1.delay(320, start);
    return helper_1.gate(canBeTap, tapCheckEvt);
}
function mouseMoveEvent(e) {
    return {
        mouseX: e.offsetX,
        mouseY: e.offsetY
    };
}
var DragType;
(function (DragType) {
    DragType[DragType["DragStart"] = 0] = "DragStart";
    DragType[DragType["Drag"] = 1] = "Drag";
    DragType[DragType["DragEnd"] = 2] = "DragEnd";
})(DragType = exports.DragType || (exports.DragType = {}));
var isEnd = function (e) {
    return e.dragType == DragType.DragEnd;
};
var updateDragType = ramda_1.curry(function (t, d) {
    return {
        dragType: t,
        dragX: d.dragX,
        dragY: d.dragY,
        deltaX: d.deltaX,
        deltaY: d.deltaY
    };
});
function distance(d1, d2) {
    if (d1 == null)
        return 0;
    var dx = d1.dragX - d2.dragX;
    var dy = d1.dragY - d2.dragY;
    return Math.sqrt(dx * dx + dy * dy);
}
function mkDragEndable(evt) {
    // wait for 2 seconds and see if there're new events
    // if not, make sure the last one is DragEnd
    var e = core_1.debounce(1500, evt);
    var f = function (e) {
        if (e.dragType != DragType.DragEnd) {
            return {
                dragType: DragType.DragEnd,
                dragX: e.dragX,
                dragY: e.dragY,
                deltaX: 0,
                deltaY: 0
            };
        }
        return null;
    };
    return core_1.merge(evt, helper_1.unwrap(core_1.map(f, e)));
}
/**
 * Drag gesture recognizer for both mouse and touch events.
 * @param start
 * @param move
 * @param end
 */
function dragged(start, move, end) {
    var startDrag = core_1.constant(true, start);
    var endDrag = core_1.constant(false, end);
    // create an adapter stream for auto end event
    var _a = adapter_1.createAdapter(), updateEnd = _a[0], possibleEnd = _a[1];
    var posEnd = core_1.constant(false, possibleEnd);
    // we're only interested in move events between start and end
    var touching = core_1.startWith(false, core_1.mergeArray([startDrag, endDrag, posEnd]));
    var mkDrag = ramda_1.curry(function (t, e) {
        return {
            dragType: t,
            dragX: e.tapX,
            dragY: e.tapY,
            deltaX: 0,
            deltaY: 0
        };
    });
    // only move events that are in between 'touching' is real move
    var realMove = core_1.map(mkDrag(DragType.Drag), helper_1.gate(touching, move));
    // make sure user did move the mouse/touch
    var checkDist = function (s, p) {
        return distance(s, p) >= 1 ? p : null;
    };
    var dstart = core_1.map(mkDrag(DragType.DragStart), start);
    var startPos = core_1.startWith(null, dstart);
    var dragMove = helper_1.unwrap(core_1.snapshot(checkDist, startPos, realMove));
    // when user is actually dragging
    var dragging = core_1.skipRepeats(core_1.startWith(false, core_1.mergeArray([
        core_1.constant(false, start),
        core_1.constant(true, dragMove),
        core_1.constant(false, end)
    ])));
    var notDragging = core_1.map(not_1.default, dragging);
    // the drag start should be the first drag event attached with start position
    var dragStart = helper_1.unwrap(core_1.sample(startPos, helper_1.gate(notDragging, dragMove)));
    // calculate the new drag end event
    var lastPos = core_1.startWith(null, dragMove);
    var lastDrag = helper_1.unwrap(core_1.sample(lastPos, helper_1.gate(dragging, end)));
    var dragEnd = core_1.map(updateDragType(DragType.DragEnd), lastDrag);
    var def = {
        dragType: DragType.DragStart,
        dragX: 0,
        dragY: 0,
        deltaX: 0,
        deltaY: 0
    };
    // merge all drag related events and do delta calculation
    var evts = core_1.mergeArray([dragStart, dragMove, dragEnd]);
    var calcDelta = function (lastE, e) {
        if (e.dragType == DragType.DragStart) {
            return e;
        }
        else {
            e.deltaX = e.dragX - lastE.dragX;
            e.deltaY = e.dragY - lastE.dragY;
            return e;
        }
    };
    var resEvt = mkDragEndable(core_1.skip(1, core_1.scan(calcDelta, def, evts)));
    // filter End event and pipe it back to the internal possibleEnd stream
    var d = core_1.filter(isEnd, resEvt).run(sink_1.mkSink(updateEnd), helper_1.defScheduler());
    return [resEvt, d];
}
/**
 * Setup the input system for an element. It will return the InputEvents object
 * with all supported event streams.
 * @param elem
 */
function setupInput(elem) {
    var mouseTap = function (e) {
        return {
            tapX: e.offsetX,
            tapY: e.offsetY
        };
    };
    var shifted = function (e) {
        return e.getModifierState('Shift');
    };
    var notShifted = function (e) {
        return !e.getModifierState('Shift');
    };
    var touchTap = function (e) {
        var t = e.touches[0];
        var rect = elem.getBoundingClientRect();
        return {
            tapX: t.pageX - rect.left,
            tapY: t.clientY - rect.top
        };
    };
    var md = core_1.multicast(dom_event_1.mousedown(elem));
    var mm = core_1.multicast(dom_event_1.mousemove(elem));
    var mu = core_1.multicast(dom_event_1.mouseup(elem));
    var mouseStart = core_1.map(mouseTap, core_1.filter(notShifted, md));
    var mouseMove = core_1.map(mouseTap, core_1.filter(notShifted, mm));
    var mouseEnd = core_1.map(mouseTap, mu);
    var touchStart = core_1.map(touchTap, dom_event_1.touchstart(elem));
    var touchMove = core_1.map(touchTap, dom_event_1.touchmove(elem));
    var touchEnd = core_1.map(touchTap, dom_event_1.touchend(elem));
    var shiftStart = core_1.map(mouseTap, core_1.filter(shifted, md));
    var shiftMove = core_1.map(mouseTap, core_1.filter(shifted, mm));
    var start = core_1.multicast(core_1.merge(mouseStart, touchStart));
    var move = core_1.multicast(core_1.merge(mouseMove, touchMove));
    var end = core_1.multicast(core_1.merge(mouseEnd, touchEnd));
    var _a = dragged(start, move, end), drag = _a[0], disp = _a[1];
    var _b = dragged(shiftStart, shiftMove, mouseEnd), shiftDrag = _b[0], shiftDisp = _b[1];
    return {
        tapped: core_1.multicast(tapped(start, end)),
        zoomed: core_1.multicast(dom_event_1.domEvent('wheel', elem)),
        dragged: core_1.multicast(drag),
        shiftDragged: core_1.multicast(shiftDrag),
        mouseMove: core_1.multicast(core_1.map(mouseMoveEvent, dom_event_1.mousemove(elem))),
        disposable: disposable_1.disposeBoth(disp, shiftDisp)
    };
}
exports.setupInput = setupInput;
//# sourceMappingURL=input.js.map