// Generated by purs version 0.13.6
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Compactable = require("../Data.Compactable/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Int = require("../Data.Int/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Editor_Input = require("../Editor.Input/index.js");
var Effect = require("../Effect/index.js");
var FRP_Event = require("../FRP.Event/index.js");
var Three_Core_Raycaster = require("../Three.Core.Raycaster/index.js");
var Three_Math_Vector = require("../Three.Math.Vector/index.js");
var Util = require("../Util/index.js");
var stopTappable = Util.fpi([ "obj", "" ])("obj.tapped = undefined");
var stopMouseMove = Util.fpi([ "obj", "" ])("obj.mouseMove = undefined");
var stopDraggable = Util.fpi([ "obj", "" ])("obj.dragged = undefined");
var sendTapEvent = Util.fpi([ "obj", "evt", "" ])("obj.tapped(evt)()");
var sendMouseMoveEvent = Util.fpi([ "obj", "evt", "" ])("obj.mouseMove(evt)()");
var sendDragEvent = Util.fpi([ "obj", "evt", "" ])("obj.dragged(evt)()");
var mkDragEndable = function (evt) {
    var f = function (d) {
        var $2 = Data_Eq.notEq(Editor_Input.eqDragType)(d.type)(Editor_Input.DragEnd.value);
        if ($2) {
            return new Data_Maybe.Just({
                type: Editor_Input.DragEnd.value,
                distance: d.distance,
                point: d.point
            });
        };
        return Data_Maybe.Nothing.value;
    };
    var e = Util.debounce(2000.0)(evt);
    return Control_Alt.alt(FRP_Event.altEvent)(evt)(Data_Compactable.compact(FRP_Event.compactableEvent)(Data_Functor.map(FRP_Event.functorEvent)(f)(e)));
};
var makeTappable = Util.fpi([ "obj", "cb", "" ])("obj.tapped = cb");
var makeMouseMove = Util.fpi([ "obj", "cb", "" ])("obj.mouseMove = cb");
var makeDraggable = Util.fpi([ "obj", "cb", "" ])("obj.dragged = cb");
var isTappable = Util.ffi([ "obj" ])("obj.tapped !== undefined");
var processTapObjects = function (domPos) {
    return function (objs) {
        var target = Data_Array.head(Data_Array.filter(function ($5) {
            return isTappable(Three_Core_Raycaster.object($5));
        })(objs));
        var doTap = function (o) {
            return sendTapEvent(Three_Core_Raycaster.object(o))({
                distance: Three_Core_Raycaster.distance(o),
                point: Three_Core_Raycaster.point(o),
                domPosition: domPos
            });
        };
        return Data_Functor["void"](Effect.functorEffect)(Data_Traversable.traverse(Data_Traversable.traversableMaybe)(Effect.applicativeEffect)(doTap)(target));
    };
};
var isMouseMove = Util.ffi([ "obj" ])("obj.mouseMove !== undefined");
var processMouseOverObjects = function (domPos) {
    return function (objs) {
        var target = Data_Array.head(Data_Array.filter(function ($6) {
            return isMouseMove(Three_Core_Raycaster.object($6));
        })(objs));
        var doMove = function (o) {
            return sendMouseMoveEvent(Three_Core_Raycaster.object(o))({
                distance: Three_Core_Raycaster.distance(o),
                point: Three_Core_Raycaster.point(o),
                face: Three_Core_Raycaster.face(o),
                domPosition: domPos
            });
        };
        return Data_Functor["void"](Effect.functorEffect)(Data_Traversable.traverse(Data_Traversable.traversableMaybe)(Effect.applicativeEffect)(doMove)(target));
    };
};
var isDraggable = Util.ffi([ "obj" ])("obj.dragged !== undefined");
var processDragObjects = function (e) {
    return function (objs) {
        var target = Data_Array.head(Data_Array.filter(function ($7) {
            return isDraggable(Three_Core_Raycaster.object($7));
        })(objs));
        var f = function (v) {
            if (v instanceof Data_Maybe.Just) {
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Data_Maybe.Nothing) {
                return new Data_Maybe.Just(e);
            };
            throw new Error("Failed pattern match at Editor.SceneEvent (line 158, column 11 - line 158, column 31): " + [ v.constructor.name ]);
        };
        var doDrag = function (o) {
            return sendDragEvent(Three_Core_Raycaster.object(o))({
                type: e.dragType,
                distance: Three_Core_Raycaster.distance(o),
                point: Three_Core_Raycaster.point(o)
            });
        };
        return Control_Apply.applySecond(Effect.applyEffect)(Data_Traversable.traverse(Data_Traversable.traversableMaybe)(Effect.applicativeEffect)(doDrag)(target))(Control_Applicative.pure(Effect.applicativeEffect)(f(target)));
    };
};
var isDragStart = function (e) {
    return Data_Eq.eq(Editor_Input.eqDragType)(e.type)(Editor_Input.DragStart.value);
};
var isDragEnd = function (e) {
    return Data_Eq.eq(Editor_Input.eqDragType)(e.type)(Editor_Input.DragEnd.value);
};
var isDrag = function (e) {
    return Data_Eq.eq(Editor_Input.eqDragType)(e.type)(Editor_Input.Drag.value);
};
var calcPosition = function (s) {
    return function (pos) {
        var y = -(Three_Math_Vector.vecY(Three_Math_Vector.hasYVec2)(pos) / Data_Int.toNumber(s.height)) * 2.0 + 1.0;
        var x = (Three_Math_Vector.vecX(Three_Math_Vector.hasXVec2)(pos) / Data_Int.toNumber(s.width)) * 2.0 - 1.0;
        return Three_Math_Vector.mkVec2(x)(y);
    };
};
var dragPosition = function (s) {
    return function (e) {
        return calcPosition(s)(Three_Math_Vector.mkVec2(e.dragX)(e.dragY));
    };
};
var mousePosition = function (s) {
    return function (e) {
        return calcPosition(s)(Three_Math_Vector.mkVec2(e.mouseX)(e.mouseY));
    };
};
var tapPosition = function (s) {
    return function (e) {
        return calcPosition(s)(Three_Math_Vector.mkVec2(e.tapX)(e.tapY));
    };
};
var setupRaycasting = function (camera) {
    return function (scene) {
        return function (input) {
            return function (size) {
                return function __do() {
                    var raycaster = Three_Core_Raycaster.mkRaycaster();
                    var doRaycast = function (tp) {
                        return function __do() {
                            Three_Core_Raycaster.setFromCamera(raycaster)(tp)(camera)();
                            return Three_Core_Raycaster.intersectObject(raycaster)(scene)(true)();
                        };
                    };
                    var raycastDrag = function (sz) {
                        return function (e) {
                            return function __do() {
                                var res = doRaycast(dragPosition(sz)(e))();
                                return processDragObjects(e)(res)();
                            };
                        };
                    };
                    var raycastMouse = function (sz) {
                        return function (e) {
                            var domPos = Three_Math_Vector.mkVec2(e.mouseX)(e.mouseY);
                            return function __do() {
                                var res = doRaycast(mousePosition(sz)(e))();
                                return processMouseOverObjects(domPos)(res)();
                            };
                        };
                    };
                    var raycastTap = function (sz) {
                        return function (e) {
                            var domPos = Three_Math_Vector.mkVec2(e.tapX)(e.tapY);
                            return function __do() {
                                var res = doRaycast(tapPosition(sz)(e))();
                                return processTapObjects(domPos)(res)();
                            };
                        };
                    };
                    var unraycastedDrag = Data_Compactable.compact(FRP_Event.compactableEvent)(Util.performEvent(Control_Apply.lift2(FRP_Event.applyEvent)(raycastDrag)(size)(input.dragged)));
                    var f = function (v) {
                        return Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit);
                    };
                    var e2 = Util.performEvent(Control_Apply.lift2(FRP_Event.applyEvent)(raycastMouse)(size)(input.mouseMove));
                    var e1 = Util.performEvent(Control_Apply.lift2(FRP_Event.applyEvent)(raycastTap)(size)(input.tapped));
                    var d1 = FRP_Event.subscribe(e1)(f)();
                    var d2 = FRP_Event.subscribe(e2)(f)();
                    return {
                        dragEvent: Util.multicast(unraycastedDrag),
                        dispose: Data_Foldable.sequence_(Effect.applicativeEffect)(Data_Foldable.foldableArray)([ d1, d2 ])
                    };
                };
            };
        };
    };
};
module.exports = {
    isDragStart: isDragStart,
    isDrag: isDrag,
    isDragEnd: isDragEnd,
    mkDragEndable: mkDragEndable,
    makeTappable: makeTappable,
    stopTappable: stopTappable,
    isTappable: isTappable,
    sendTapEvent: sendTapEvent,
    makeMouseMove: makeMouseMove,
    stopMouseMove: stopMouseMove,
    isMouseMove: isMouseMove,
    sendMouseMoveEvent: sendMouseMoveEvent,
    makeDraggable: makeDraggable,
    stopDraggable: stopDraggable,
    isDraggable: isDraggable,
    sendDragEvent: sendDragEvent,
    calcPosition: calcPosition,
    tapPosition: tapPosition,
    mousePosition: mousePosition,
    dragPosition: dragPosition,
    processTapObjects: processTapObjects,
    processMouseOverObjects: processMouseOverObjects,
    processDragObjects: processDragObjects,
    setupRaycasting: setupRaycasting
};