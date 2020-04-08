// Generated by purs version 0.13.6
"use strict";
var $foreign = require("./foreign.js");
var Control_Alt = require("../Control.Alt/index.js");
var Data_Compactable = require("../Data.Compactable/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Filterable = require("../Data.Filterable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Generic_Rep = require("../Data.Generic.Rep/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Int = require("../Data.Int/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var FRP_Behavior = require("../FRP.Behavior/index.js");
var FRP_Event = require("../FRP.Event/index.js");
var FRP_Event_Class = require("../FRP.Event.Class/index.js");
var FRP_Event_Time = require("../FRP.Event.Time/index.js");
var $$Math = require("../Math/index.js");
var Util = require("../Util/index.js");
var Web_DOM_Element = require("../Web.DOM.Element/index.js");
var Web_Event_EventTarget = require("../Web.Event.EventTarget/index.js");
var Web_TouchEvent_EventTypes = require("../Web.TouchEvent.EventTypes/index.js");
var Web_TouchEvent_Touch = require("../Web.TouchEvent.Touch/index.js");
var Web_TouchEvent_TouchEvent = require("../Web.TouchEvent.TouchEvent/index.js");
var Web_TouchEvent_TouchList = require("../Web.TouchEvent.TouchList/index.js");
var Web_UIEvent_MouseEvent = require("../Web.UIEvent.MouseEvent/index.js");
var Web_UIEvent_MouseEvent_EventTypes = require("../Web.UIEvent.MouseEvent.EventTypes/index.js");
var Web_UIEvent_WheelEvent = require("../Web.UIEvent.WheelEvent/index.js");
var Web_UIEvent_WheelEvent_EventTypes = require("../Web.UIEvent.WheelEvent.EventTypes/index.js");
var DragStart = (function () {
    function DragStart() {

    };
    DragStart.value = new DragStart();
    return DragStart;
})();
var Drag = (function () {
    function Drag() {

    };
    Drag.value = new Drag();
    return Drag;
})();
var DragEnd = (function () {
    function DragEnd() {

    };
    DragEnd.value = new DragEnd();
    return DragEnd;
})();
var wheelEvent = function (target) {
    return Data_Compactable.compact(FRP_Event.compactableEvent)(FRP_Event.makeEvent(function (k) {
        return function __do() {
            var listener = Web_Event_EventTarget.eventListener(function (e) {
                return k(Web_UIEvent_WheelEvent.fromEvent(e));
            })();
            Web_Event_EventTarget.addEventListener(Web_UIEvent_WheelEvent_EventTypes.wheel)(listener)(false)(target)();
            return Web_Event_EventTarget.removeEventListener(Web_UIEvent_WheelEvent_EventTypes.wheel)(listener)(false)(target);
        };
    }));
};
var updateDragType = function (t) {
    return function (e) {
        return {
            dragType: t,
            dragX: e.dragX,
            dragY: e.dragY,
            deltaX: e.deltaX,
            deltaY: e.deltaY
        };
    };
};
var touchTap = function (elem) {
    return function (e) {
        var rect = $foreign.getBoundingClientRect(elem);
        var getY = function (t) {
            return Data_Int.toNumber(Web_TouchEvent_Touch.clientY(t)) - rect.top;
        };
        var getX = function (t) {
            return Data_Int.toNumber(Web_TouchEvent_Touch.pageX(t)) - rect.left;
        };
        var tapT = function (t) {
            return {
                tapX: getX(t),
                tapY: getY(t)
            };
        };
        return Data_Functor.map(Data_Maybe.functorMaybe)(tapT)(Web_TouchEvent_TouchList.item(0)(Web_TouchEvent_TouchEvent.touches(e)));
    };
};
var touchEvent = function (t) {
    return function (target) {
        return Data_Compactable.compact(FRP_Event.compactableEvent)(FRP_Event.makeEvent(function (k) {
            return function __do() {
                var listener = Web_Event_EventTarget.eventListener(function (e) {
                    return k(Web_TouchEvent_TouchEvent.fromEvent(e));
                })();
                Web_Event_EventTarget.addEventListener(t)(listener)(false)(target)();
                return Web_Event_EventTarget.removeEventListener(t)(listener)(false)(target);
            };
        }));
    };
};
var tapped = function (start) {
    return function (end) {
        var tapCheck = Util.delay(320)(start);
        var s = Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](false))(start);
        var e = Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](true))(end);
        var canBeTap = FRP_Behavior.step(FRP_Event.eventIsEvent)(false)(Control_Alt.alt(FRP_Event.altEvent)(s)(e));
        return FRP_Behavior.gate(FRP_Event.eventIsEvent)(canBeTap)(tapCheck);
    };
};
var offsetY = Util.ffi([ "mouseEvt" ])("mouseEvt.offsetY");
var offsetX = Util.ffi([ "mouseEvt" ])("mouseEvt.offsetX");
var mouseTap = function (e) {
    return {
        tapX: offsetX(e),
        tapY: offsetY(e)
    };
};
var mouseMoveEvent = function (e) {
    return {
        mouseX: offsetX(e),
        mouseY: offsetY(e)
    };
};
var mouseEvent = function (t) {
    return function (target) {
        return Data_Compactable.compact(FRP_Event.compactableEvent)(FRP_Event.makeEvent(function (k) {
            return function __do() {
                var listener = Web_Event_EventTarget.eventListener(function (e) {
                    return k(Web_UIEvent_MouseEvent.fromEvent(e));
                })();
                Web_Event_EventTarget.addEventListener(t)(listener)(false)(target)();
                return Web_Event_EventTarget.removeEventListener(t)(listener)(false)(target);
            };
        }));
    };
};
var genericDragType = new Data_Generic_Rep.Generic(function (x) {
    if (x instanceof DragStart) {
        return new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value);
    };
    if (x instanceof Drag) {
        return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value));
    };
    if (x instanceof DragEnd) {
        return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(Data_Generic_Rep.NoArguments.value));
    };
    throw new Error("Failed pattern match at Editor.Input (line 62, column 1 - line 62, column 54): " + [ x.constructor.name ]);
}, function (x) {
    if (x instanceof Data_Generic_Rep.Inl) {
        return DragStart.value;
    };
    if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inl) {
        return Drag.value;
    };
    if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inr) {
        return DragEnd.value;
    };
    throw new Error("Failed pattern match at Editor.Input (line 62, column 1 - line 62, column 54): " + [ x.constructor.name ]);
});
var eqDragType = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof DragStart && y instanceof DragStart) {
            return true;
        };
        if (x instanceof Drag && y instanceof Drag) {
            return true;
        };
        if (x instanceof DragEnd && y instanceof DragEnd) {
            return true;
        };
        return false;
    };
});
var isEnd = function (e) {
    return Data_Eq.eq(eqDragType)(e.dragType)(DragEnd.value);
};
var mkDragEndable = function (evt) {
    var f = function (d) {
        var $14 = Data_Eq.notEq(eqDragType)(d.dragType)(DragEnd.value);
        if ($14) {
            return new Data_Maybe.Just({
                dragType: DragEnd.value,
                dragX: d.dragX,
                dragY: d.dragY,
                deltaX: 0.0,
                deltaY: 0.0
            });
        };
        return Data_Maybe.Nothing.value;
    };
    var e = FRP_Event_Time.debounce(1500.0)(evt);
    return Control_Alt.alt(FRP_Event.altEvent)(evt)(Data_Compactable.compact(FRP_Event.compactableEvent)(Data_Functor.map(FRP_Event.functorEvent)(f)(e)));
};
var distance = function (e1) {
    return function (e2) {
        var dy = e1.dragY - e2.dragY;
        var dx = e1.dragX - e2.dragX;
        return $$Math.sqrt(dx * dx + dy * dy);
    };
};
var dragged = function (start) {
    return function (move) {
        return function (end) {
            return FRP_Event_Class.fix(FRP_Event.eventIsEvent)(function (possibleEnd) {
                var startDrag = Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](true))(start);
                var posEnd = Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](false))(possibleEnd);
                var mkDrag = function (t) {
                    return function (e) {
                        return {
                            dragType: t,
                            dragX: e.tapX,
                            dragY: e.tapY,
                            deltaX: 0.0,
                            deltaY: 0.0
                        };
                    };
                };
                var endDrag = Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](false))(end);
                var touching = FRP_Behavior.step(FRP_Event.eventIsEvent)(false)(Control_Alt.alt(FRP_Event.altEvent)(Control_Alt.alt(FRP_Event.altEvent)(startDrag)(endDrag))(posEnd));
                var realMove = Data_Functor.map(FRP_Event.functorEvent)(mkDrag(Drag.value))(FRP_Behavior.gate(FRP_Event.eventIsEvent)(touching)(move));
                var dstart = Data_Functor.map(FRP_Event.functorEvent)(mkDrag(DragStart.value))(start);
                var startPos = FRP_Behavior.step(FRP_Event.eventIsEvent)(Data_Maybe.Nothing.value)(Data_Functor.map(FRP_Event.functorEvent)(Data_Maybe.Just.create)(dstart));
                var def = {
                    dragType: DragStart.value,
                    dragX: 0.0,
                    dragY: 0.0,
                    deltaX: 0.0,
                    deltaY: 0.0
                };
                var checkDist = function (v) {
                    return function (p) {
                        if (v instanceof Data_Maybe.Just) {
                            var $17 = distance(v.value0)(p) >= 1.0;
                            if ($17) {
                                return new Data_Maybe.Just(p);
                            };
                            return Data_Maybe.Nothing.value;
                        };
                        if (v instanceof Data_Maybe.Nothing) {
                            return Data_Maybe.Nothing.value;
                        };
                        throw new Error("Failed pattern match at Editor.Input (line 119, column 11 - line 119, column 81): " + [ v.constructor.name, p.constructor.name ]);
                    };
                };
                var dragMove = Data_Compactable.compact(FRP_Event.compactableEvent)(FRP_Behavior.sampleBy(FRP_Event.eventIsEvent)(checkDist)(startPos)(realMove));
                var dragging = FRP_Behavior.step(FRP_Event.eventIsEvent)(false)(Control_Alt.alt(FRP_Event.altEvent)(Control_Alt.alt(FRP_Event.altEvent)(Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](false))(start))(Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](true))(dragMove)))(Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](false))(end)));
                var notDragging = Data_Functor.map(FRP_Behavior.functorABehavior(FRP_Event.functorEvent))(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean))(dragging);
                var dragStart = Data_Compactable.compact(FRP_Event.compactableEvent)(FRP_Behavior.sample_(FRP_Event.eventIsEvent)(startPos)(FRP_Behavior.gate(FRP_Event.eventIsEvent)(notDragging)(dragMove)));
                var lastPos = FRP_Behavior.step(FRP_Event.eventIsEvent)(Data_Maybe.Nothing.value)(Data_Functor.map(FRP_Event.functorEvent)(Data_Maybe.Just.create)(dragMove));
                var lastDrag = Data_Compactable.compact(FRP_Event.compactableEvent)(FRP_Behavior.sample_(FRP_Event.eventIsEvent)(lastPos)(FRP_Behavior.gate(FRP_Event.eventIsEvent)(dragging)(end)));
                var dragEnd = Data_Functor.map(FRP_Event.functorEvent)(updateDragType(DragEnd.value))(lastDrag);
                var evts = Control_Alt.alt(FRP_Event.altEvent)(Control_Alt.alt(FRP_Event.altEvent)(dragStart)(dragMove))(dragEnd);
                var calcDelta = function (v) {
                    if (v.last instanceof Data_Maybe.Just) {
                        return {
                            dragType: v.now.dragType,
                            dragX: v.now.dragX,
                            dragY: v.now.dragY,
                            deltaX: v.now.dragX - v.last.value0.dragX,
                            deltaY: v.now.dragY - v.last.value0.dragY
                        };
                    };
                    if (v.last instanceof Data_Maybe.Nothing) {
                        return v.now;
                    };
                    throw new Error("Failed pattern match at Editor.Input (line 142, column 37 - line 149, column 51): " + [ v.last.constructor.name ]);
                };
                var resEvt = mkDragEndable(Util.skip(1)(Data_Functor.map(FRP_Event.functorEvent)(calcDelta)(FRP_Event_Class.withLast(FRP_Event.eventIsEvent)(evts))));
                return {
                    input: Data_Filterable.filter(FRP_Event.filterableEvent)(isEnd)(resEvt),
                    output: resEvt
                };
            });
        };
    };
};
var setupInput = function (elem) {
    var target = Web_DOM_Element.toEventTarget(elem);
    var touchEnd = Data_Compactable.compact(FRP_Event.compactableEvent)(Data_Functor.map(FRP_Event.functorEvent)(touchTap(elem))(touchEvent(Web_TouchEvent_EventTypes.touchend)(target)));
    var touchMove = Data_Compactable.compact(FRP_Event.compactableEvent)(Data_Functor.map(FRP_Event.functorEvent)(touchTap(elem))(touchEvent(Web_TouchEvent_EventTypes.touchmove)(target)));
    var touchStart = Data_Compactable.compact(FRP_Event.compactableEvent)(Data_Functor.map(FRP_Event.functorEvent)(touchTap(elem))(touchEvent(Web_TouchEvent_EventTypes.touchstart)(target)));
    var wheelEvt = wheelEvent(target);
    var mu = mouseEvent(Web_UIEvent_MouseEvent_EventTypes.mouseup)(target);
    var mouseEnd = Data_Functor.map(FRP_Event.functorEvent)(mouseTap)(mu);
    var mm = mouseEvent(Web_UIEvent_MouseEvent_EventTypes.mousemove)(target);
    var mouseMove = Data_Functor.map(FRP_Event.functorEvent)(mouseTap)(Data_Filterable.filter(FRP_Event.filterableEvent)((function () {
        var $24 = Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean);
        return function ($25) {
            return $24(Web_UIEvent_MouseEvent.shiftKey($25));
        };
    })())(mm));
    var move = Control_Alt.alt(FRP_Event.altEvent)(mouseMove)(touchMove);
    var shiftMove = Data_Functor.map(FRP_Event.functorEvent)(mouseTap)(Data_Filterable.filter(FRP_Event.filterableEvent)(Web_UIEvent_MouseEvent.shiftKey)(mm));
    var md = mouseEvent(Web_UIEvent_MouseEvent_EventTypes.mousedown)(target);
    var mouseStart = Data_Functor.map(FRP_Event.functorEvent)(mouseTap)(Data_Filterable.filter(FRP_Event.filterableEvent)((function () {
        var $26 = Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean);
        return function ($27) {
            return $26(Web_UIEvent_MouseEvent.shiftKey($27));
        };
    })())(md));
    var start = Control_Alt.alt(FRP_Event.altEvent)(mouseStart)(touchStart);
    var shiftStart = Data_Functor.map(FRP_Event.functorEvent)(mouseTap)(Data_Filterable.filter(FRP_Event.filterableEvent)(Web_UIEvent_MouseEvent.shiftKey)(md));
    var shiftDrag = dragged(shiftStart)(shiftMove)(mouseEnd);
    var end = Control_Alt.alt(FRP_Event.altEvent)(mouseEnd)(touchEnd);
    var drag = dragged(start)(move)(end);
    return {
        tapped: tapped(start)(end),
        zoomed: wheelEvt,
        dragged: drag,
        shiftDragged: shiftDrag,
        mouseMove: Data_Functor.map(FRP_Event.functorEvent)(mouseMoveEvent)(mm)
    };
};
module.exports = {
    tapped: tapped,
    mouseMoveEvent: mouseMoveEvent,
    DragStart: DragStart,
    Drag: Drag,
    DragEnd: DragEnd,
    isEnd: isEnd,
    updateDragType: updateDragType,
    distance: distance,
    mkDragEndable: mkDragEndable,
    dragged: dragged,
    mouseEvent: mouseEvent,
    touchEvent: touchEvent,
    wheelEvent: wheelEvent,
    mouseTap: mouseTap,
    offsetX: offsetX,
    offsetY: offsetY,
    touchTap: touchTap,
    setupInput: setupInput,
    genericDragType: genericDragType,
    eqDragType: eqDragType,
    getBoundingClientRect: $foreign.getBoundingClientRect
};
