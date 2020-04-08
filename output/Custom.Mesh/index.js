// Generated by purs version 0.13.6
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Compactable = require("../Data.Compactable/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Editor_Input = require("../Editor.Input/index.js");
var Editor_SceneEvent = require("../Editor.SceneEvent/index.js");
var Effect = require("../Effect/index.js");
var FRP_Event = require("../FRP.Event/index.js");
var FRP_Event_Class = require("../FRP.Event.Class/index.js");
var Three_Core_Mesh = require("../Three.Core.Mesh/index.js");
var Three_Core_Object3D = require("../Three.Core.Object3D/index.js");
var Three_Math_Vector = require("../Three.Math.Vector/index.js");
var Util = require("../Util/index.js");
var validateDrag = function (evt) {
    var f = function (e) {
        return function (canDrag) {
            if (Data_Eq.eq(Editor_Input.eqDragType)(e.type)(Editor_Input.DragStart.value)) {
                if (canDrag) {
                    return new Data_Tuple.Tuple(true, Data_Maybe.Nothing.value);
                };
                return new Data_Tuple.Tuple(true, new Data_Maybe.Just(e));
            };
            if (canDrag && Data_Eq.eq(Editor_Input.eqDragType)(e.type)(Editor_Input.Drag.value)) {
                return new Data_Tuple.Tuple(true, new Data_Maybe.Just(e));
            };
            if (Data_Eq.eq(Editor_Input.eqDragType)(e.type)(Editor_Input.DragEnd.value)) {
                if (canDrag) {
                    return new Data_Tuple.Tuple(false, new Data_Maybe.Just(e));
                };
                return new Data_Tuple.Tuple(false, Data_Maybe.Nothing.value);
            };
            if (Data_Boolean.otherwise) {
                return new Data_Tuple.Tuple(false, Data_Maybe.Nothing.value);
            };
            throw new Error("Failed pattern match at Custom.Mesh (line 42, column 11 - line 49, column 56): " + [ e.constructor.name, canDrag.constructor.name ]);
        };
    };
    return Data_Compactable.compact(FRP_Event.compactableEvent)(FRP_Event_Class.mapAccum(FRP_Event.eventIsEvent)(f)(evt)(false));
};
var toLocal = function (mesh) {
    return function (v) {
        var $4 = Three_Core_Object3D.hasParent(mesh);
        if ($4) {
            return Data_Functor.map(Effect.functorEffect)(Data_Maybe.Just.create)(Three_Core_Object3D.worldToLocal(v)(Three_Core_Object3D.parent(mesh)));
        };
        return Control_Applicative.pure(Effect.applicativeEffect)(Data_Maybe.Nothing.value);
    };
};
var tapEvtOn = function (m) {
    return FRP_Event.makeEvent(function (k) {
        return function __do() {
            Editor_SceneEvent.makeTappable(m)(k)();
            return Editor_SceneEvent.stopTappable(m);
        };
    });
};
var mkTappableMesh = function (geo) {
    return function (mat) {
        return function __do() {
            var mesh = Three_Core_Mesh.mkMesh(geo)(mat)();
            return {
                mesh: mesh,
                tapped: tapEvtOn(mesh)
            };
        };
    };
};
var dragEvtOn = function (m) {
    return FRP_Event.makeEvent(function (k) {
        return function __do() {
            Editor_SceneEvent.makeDraggable(m)(k)();
            return Editor_SceneEvent.stopDraggable(m);
        };
    });
};
var calcDragDelta = function (toLocalF) {
    return function (evt) {
        var zero = Three_Math_Vector.mkVec3(0.0)(0.0)(0.0);
        var mkNewDrag = function (d) {
            return function (p) {
                return {
                    distance: d.distance,
                    type: d.type,
                    point: p
                };
            };
        };
        var f = function (d) {
            return Data_Functor.map(Effect.functorEffect)(Data_Functor.map(Data_Maybe.functorMaybe)(mkNewDrag(d)))(toLocalF(d.point));
        };
        var e = Data_Compactable.compact(FRP_Event.compactableEvent)(Util.performEvent(Data_Functor.map(FRP_Event.functorEvent)(f)(evt)));
        var def = {
            type: Editor_Input.DragStart.value,
            distance: 0.0,
            point: zero
        };
        var calcDelta = function (ne) {
            return function (oldE) {
                if (Data_Eq.eq(Editor_Input.eqDragType)(ne.type)(Editor_Input.DragStart.value)) {
                    return new Data_Tuple.Tuple(ne, zero);
                };
                if (Data_Boolean.otherwise) {
                    return new Data_Tuple.Tuple(ne, Three_Math_Vector.sub(Three_Math_Vector.vecVec3)(ne.point)(oldE.point));
                };
                throw new Error("Failed pattern match at Custom.Mesh (line 61, column 11 - line 62, column 88): " + [ ne.constructor.name, oldE.constructor.name ]);
            };
        };
        return FRP_Event_Class.mapAccum(FRP_Event.eventIsEvent)(calcDelta)(e)(def);
    };
};
var mkDraggableMesh = function (geo) {
    return function (mat) {
        return function __do() {
            var mesh = Three_Core_Mesh.mkMesh(geo)(mat)();
            var dragged = dragEvtOn(mesh);
            var dragDelta = calcDragDelta(toLocal(mesh))(dragged);
            return {
                mesh: mesh,
                dragged: dragged,
                dragDelta: dragDelta
            };
        };
    };
};
var mkTapDragMesh = function (geo) {
    return function (mat) {
        return function __do() {
            var m = mkDraggableMesh(geo)(mat)();
            return {
                mesh: m.mesh,
                tapped: tapEvtOn(m.mesh),
                dragged: m.dragged,
                dragDelta: m.dragDelta
            };
        };
    };
};
module.exports = {
    tapEvtOn: tapEvtOn,
    mkTappableMesh: mkTappableMesh,
    validateDrag: validateDrag,
    calcDragDelta: calcDragDelta,
    dragEvtOn: dragEvtOn,
    toLocal: toLocal,
    mkDraggableMesh: mkDraggableMesh,
    mkTapDragMesh: mkTapDragMesh
};
