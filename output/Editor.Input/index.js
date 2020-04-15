// Generated by purs version 0.13.6
"use strict";
var $foreign = require("./foreign.js");
var Control_Alt = require("../Control.Alt/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Compactable = require("../Data.Compactable/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Filterable = require("../Data.Filterable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Generic_Rep = require("../Data.Generic.Rep/index.js");
var Data_Generic_Rep_Show = require("../Data.Generic.Rep.Show/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Int = require("../Data.Int/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Effect = require("../Effect/index.js");
var FRP_Event = require("../FRP.Event/index.js");
var FRP_Event_Class = require("../FRP.Event.Class/index.js");
var FRP_Event_Extra = require("../FRP.Event.Extra/index.js");
var $$Math = require("../Math/index.js");
var Util = require("../Util/index.js");
var Web_DOM_Element = require("../Web.DOM.Element/index.js");
var Web_Event_Event = require("../Web.Event.Event/index.js");
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
                return Control_Apply.applySecond(Effect.applyEffect)(Web_Event_Event.preventDefault(e))(k(Web_UIEvent_WheelEvent.fromEvent(e)));
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
        var tapCheck = FRP_Event_Extra.delay(320)(start);
        var s = Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](false))(start);
        var e = Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](true))(end);
        var canBeTap = Control_Alt.alt(FRP_Event.altEvent)(s)(e);
        return FRP_Event_Class.gate(FRP_Event.eventIsEvent)(canBeTap)(tapCheck);
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
var showDragType = new Data_Show.Show(Data_Generic_Rep_Show.genericShow(genericDragType)(Data_Generic_Rep_Show.genericShowSum(Data_Generic_Rep_Show.genericShowConstructor(Data_Generic_Rep_Show.genericShowArgsNoArguments)(new Data_Symbol.IsSymbol(function () {
    return "DragStart";
})))(Data_Generic_Rep_Show.genericShowSum(Data_Generic_Rep_Show.genericShowConstructor(Data_Generic_Rep_Show.genericShowArgsNoArguments)(new Data_Symbol.IsSymbol(function () {
    return "Drag";
})))(Data_Generic_Rep_Show.genericShowConstructor(Data_Generic_Rep_Show.genericShowArgsNoArguments)(new Data_Symbol.IsSymbol(function () {
    return "DragEnd";
}))))));
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
        var $21 = Data_Eq.notEq(eqDragType)(d.dragType)(DragEnd.value);
        if ($21) {
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
    var e = FRP_Event_Extra.debounce(1500.0)(evt);
    return Control_Alt.alt(FRP_Event.altEvent)(evt)(Data_Compactable.compact(FRP_Event.compactableEvent)(Data_Functor.map(FRP_Event.functorEvent)(f)(e)));
};
var distance = function (e1) {
    return function (e2) {
        var dy = e1.dragY - e2.dragY;
        var dx = e1.dragX - e2.dragX;
        return $$Math.sqrt(dx * dx + dy * dy);
    };
};
var defState = {
    canDrag: false,
    isDragging: false,
    lastDragEvt: Data_Maybe.Nothing.value,
    curDragEvt: Data_Maybe.Nothing.value
};
var calcDelta = function (evt) {
    return function (oEvt) {
        return {
            dragType: evt.dragType,
            dragX: evt.dragX,
            dragY: evt.dragY,
            deltaX: evt.dragX - oEvt.dragX,
            deltaY: evt.dragY - oEvt.dragY
        };
    };
};
var processDrag = function (evt) {
    return function (st) {
        if (Data_Eq.eq(eqDragType)(evt.dragType)(DragStart.value)) {
            return {
                canDrag: true,
                isDragging: false,
                lastDragEvt: new Data_Maybe.Just(evt),
                curDragEvt: Data_Maybe.Nothing.value
            };
        };
        if (Data_Eq.eq(eqDragType)(evt.dragType)(Drag.value) && (!st.isDragging && st.canDrag)) {
            var $24 = distance(evt)(Data_Maybe.fromMaybe(evt)(st.lastDragEvt)) > 1.0;
            if ($24) {
                var nEvt = {
                    dragType: DragStart.value,
                    deltaX: evt.deltaX,
                    deltaY: evt.deltaY,
                    dragX: evt.dragX,
                    dragY: evt.dragY
                };
                return {
                    canDrag: st.canDrag,
                    isDragging: true,
                    lastDragEvt: new Data_Maybe.Just(nEvt),
                    curDragEvt: new Data_Maybe.Just(nEvt)
                };
            };
            return {
                canDrag: st.canDrag,
                isDragging: st.isDragging,
                lastDragEvt: new Data_Maybe.Just(evt),
                curDragEvt: st.curDragEvt
            };
        };
        if (Data_Eq.eq(eqDragType)(evt.dragType)(Drag.value) && st.isDragging) {
            var oEvt = Data_Maybe.fromMaybe(evt)(st.lastDragEvt);
            var nEvt = calcDelta(evt)(oEvt);
            return {
                canDrag: st.canDrag,
                isDragging: st.isDragging,
                lastDragEvt: new Data_Maybe.Just(nEvt),
                curDragEvt: new Data_Maybe.Just(nEvt)
            };
        };
        if (Data_Eq.eq(eqDragType)(evt.dragType)(DragEnd.value) && st.isDragging) {
            var oEvt = Data_Maybe.fromMaybe(evt)(st.lastDragEvt);
            var nEvt = calcDelta(evt)(oEvt);
            return {
                canDrag: false,
                isDragging: false,
                lastDragEvt: new Data_Maybe.Just(nEvt),
                curDragEvt: new Data_Maybe.Just(nEvt)
            };
        };
        if (Data_Eq.eq(eqDragType)(evt.dragType)(DragEnd.value) && (!st.isDragging && st.canDrag)) {
            return {
                canDrag: false,
                isDragging: st.isDragging,
                lastDragEvt: new Data_Maybe.Just(evt),
                curDragEvt: Data_Maybe.Nothing.value
            };
        };
        if (Data_Boolean.otherwise) {
            return {
                canDrag: st.canDrag,
                isDragging: st.isDragging,
                lastDragEvt: new Data_Maybe.Just(evt),
                curDragEvt: Data_Maybe.Nothing.value
            };
        };
        throw new Error("Failed pattern match at Editor.Input (line 121, column 1 - line 121, column 51): " + [ evt.constructor.name, st.constructor.name ]);
    };
};
var dragged = function (start) {
    return function (move) {
        return function (end) {
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
            var dragStart = Data_Functor.map(FRP_Event.functorEvent)(mkDrag(DragStart.value))(start);
            var dragMove = Data_Functor.map(FRP_Event.functorEvent)(mkDrag(Drag.value))(move);
            var dragEnd = Data_Functor.map(FRP_Event.functorEvent)(mkDrag(DragEnd.value))(end);
            var evts = mkDragEndable(Control_Alt.alt(FRP_Event.altEvent)(Control_Alt.alt(FRP_Event.altEvent)(dragStart)(dragMove))(dragEnd));
            return Data_Compactable.compact(FRP_Event.compactableEvent)(Data_Functor.map(FRP_Event.functorEvent)(function (v) {
                return v.curDragEvt;
            })(FRP_Event_Class.fold(FRP_Event.eventIsEvent)(processDrag)(evts)(defState)));
        };
    };
};
var setupInput = function (elem) {
    var target = Web_DOM_Element.toEventTarget(elem);
    var touchEnd = Data_Compactable.compact(FRP_Event.compactableEvent)(Data_Functor.map(FRP_Event.functorEvent)(touchTap(elem))(FRP_Event_Extra.multicast(touchEvent(Web_TouchEvent_EventTypes.touchend)(target))));
    var touchMove = Data_Compactable.compact(FRP_Event.compactableEvent)(Data_Functor.map(FRP_Event.functorEvent)(touchTap(elem))(FRP_Event_Extra.multicast(touchEvent(Web_TouchEvent_EventTypes.touchmove)(target))));
    var touchStart = Data_Compactable.compact(FRP_Event.compactableEvent)(Data_Functor.map(FRP_Event.functorEvent)(touchTap(elem))(FRP_Event_Extra.multicast(touchEvent(Web_TouchEvent_EventTypes.touchstart)(target))));
    var wheelEvt = FRP_Event_Extra.multicast(wheelEvent(target));
    var mu = FRP_Event_Extra.multicast(mouseEvent(Web_UIEvent_MouseEvent_EventTypes.mouseup)(target));
    var mouseEnd = Data_Functor.map(FRP_Event.functorEvent)(mouseTap)(mu);
    var mm = FRP_Event_Extra.multicast(mouseEvent(Web_UIEvent_MouseEvent_EventTypes.mousemove)(target));
    var mouseMove = Data_Functor.map(FRP_Event.functorEvent)(mouseTap)(Data_Filterable.filter(FRP_Event.filterableEvent)((function () {
        var $25 = Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean);
        return function ($26) {
            return $25(Web_UIEvent_MouseEvent.shiftKey($26));
        };
    })())(mm));
    var move = FRP_Event_Extra.multicast(Control_Alt.alt(FRP_Event.altEvent)(mouseMove)(touchMove));
    var shiftMove = Data_Functor.map(FRP_Event.functorEvent)(mouseTap)(Data_Filterable.filter(FRP_Event.filterableEvent)(Web_UIEvent_MouseEvent.shiftKey)(mm));
    var md = FRP_Event_Extra.multicast(mouseEvent(Web_UIEvent_MouseEvent_EventTypes.mousedown)(target));
    var mouseStart = Data_Functor.map(FRP_Event.functorEvent)(mouseTap)(Data_Filterable.filter(FRP_Event.filterableEvent)((function () {
        var $27 = Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean);
        return function ($28) {
            return $27(Web_UIEvent_MouseEvent.shiftKey($28));
        };
    })())(md));
    var start = FRP_Event_Extra.multicast(Control_Alt.alt(FRP_Event.altEvent)(mouseStart)(touchStart));
    var shiftStart = Data_Functor.map(FRP_Event.functorEvent)(mouseTap)(Data_Filterable.filter(FRP_Event.filterableEvent)(Web_UIEvent_MouseEvent.shiftKey)(md));
    var shiftDrag = dragged(shiftStart)(shiftMove)(mouseEnd);
    var end = FRP_Event_Extra.multicast(Control_Alt.alt(FRP_Event.altEvent)(mouseEnd)(touchEnd));
    var drag = dragged(start)(move)(end);
    return {
        tapped: FRP_Event_Extra.multicast(tapped(start)(end)),
        zoomed: wheelEvt,
        dragged: FRP_Event_Extra.multicast(drag),
        shiftDragged: FRP_Event_Extra.multicast(shiftDrag),
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
    defState: defState,
    calcDelta: calcDelta,
    processDrag: processDrag,
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
    showDragType: showDragType,
    getBoundingClientRect: $foreign.getBoundingClientRect
};
