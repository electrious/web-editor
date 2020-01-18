"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var input_1 = require("./input");
var three_1 = require("three");
var sink_1 = require("./sink");
var core_1 = require("@most/core");
var disposable_1 = require("@most/disposable");
var helper_1 = require("./helper");
function isDragStart(e) {
    return e.type == input_1.DragType.DragStart;
}
exports.isDragStart = isDragStart;
function isDrag(e) {
    return e.type == input_1.DragType.Drag;
}
exports.isDrag = isDrag;
function isDragEnd(e) {
    return e.type == input_1.DragType.DragEnd;
}
exports.isDragEnd = isDragEnd;
/**
 * add end event to SceneDragEvent stream if there's no input for a while and
 * no end event.
 * @param evt input event stream
 */
function mkDragEndable(evt) {
    // wait for 2 seconds and see if there're new events
    // if not, make sure the last one is DragEnd
    var e = core_1.debounce(2000, evt);
    var f = function (e) {
        if (e.type != input_1.DragType.DragEnd) {
            return {
                type: input_1.DragType.DragEnd,
                distance: e.distance,
                point: e.point
            };
        }
        return null;
    };
    return core_1.merge(evt, helper_1.unwrap(core_1.map(f, e)));
}
exports.mkDragEndable = mkDragEndable;
function toTappable(obj) {
    return 'tapped' in obj ? obj : null;
}
function toMouseMovable(obj) {
    return 'mouseMove' in obj ? obj : null;
}
function toDraggable(obj) {
    return 'dragged' in obj ? obj : null;
}
/**
 * convert mouse/touch position to values between -1 and 1
 * @param pos
 * @param size
 */
function calcPosition(pos, size) {
    var w = size[0];
    var h = size[1];
    return new three_1.Vector2((pos.x / w) * 2 - 1, -(pos.y / h) * 2 + 1);
}
/**
 * convert a TapEvent to position used for raycasting
 * @param size
 * @param e
 */
function tapPosition(size, e) {
    return calcPosition(new three_1.Vector2(e.tapX, e.tapY), size);
}
function mousePosition(size, e) {
    return calcPosition(new three_1.Vector2(e.mouseX, e.mouseY), size);
}
/**
 * convert a DragEvent to position used for raycasting
 * @param size
 * @param e
 */
function dragPosition(size, e) {
    return calcPosition(new three_1.Vector2(e.dragX, e.dragY), size);
}
/**
 * Find first object in the intersections array that is Tappable and send the
 * event to it.
 * @param objs
 */
function processTapObjects(domPos, objs) {
    for (var _i = 0, objs_1 = objs; _i < objs_1.length; _i++) {
        var res = objs_1[_i];
        var t = toTappable(res.object);
        if (t != null) {
            t.tapped({
                distance: res.distance,
                point: res.point,
                domPosition: domPos
            });
            break;
        }
    }
}
function processMouseOverObjects(domPos, objs) {
    for (var _i = 0, objs_2 = objs; _i < objs_2.length; _i++) {
        var obj = objs_2[_i];
        var t = toMouseMovable(obj.object);
        var f = obj.face;
        if (t != null && f != null && f != undefined) {
            t.mouseMove({
                distance: obj.distance,
                point: obj.point,
                // it's actually in local coord here.
                face: f,
                domPosition: domPos
            });
            break;
        }
    }
}
/**
 * Find first object in the intersections array that is Draggable and send the
 * drag event to it
 * @param objs
 * @param e
 */
function processDragObjects(objs, e) {
    for (var _i = 0, objs_3 = objs; _i < objs_3.length; _i++) {
        var res = objs_3[_i];
        var t = toDraggable(res.object);
        if (t != null) {
            t.dragged({
                type: e.dragType,
                distance: res.distance,
                point: res.point
            });
            return null;
        }
    }
    return e;
}
/**
 * setup all raycasting needed to process user inputs and send them to the
 * corresponding 3D object in the scene
 * @param camera threejs camera
 * @param scene threejs scene graph
 * @param input user input event streams
 * @param size stream of the current threejs viewport size
 * @param scheduler used for all stream
 */
function setupRaycasting(camera, scene, input, size) {
    var raycaster = new three_1.Raycaster();
    // function to do real raycasting
    var doRaycast = function (tp) {
        raycaster.setFromCamera(tp, camera);
        return raycaster.intersectObject(scene, true);
    };
    // raycast tap events
    var raycastTap = function (s, e) {
        var domPos = new three_1.Vector2(e.tapX, e.tapY);
        var raycastRes = doRaycast(tapPosition(s, e));
        processTapObjects(domPos, raycastRes);
    };
    // raycast mouseover events
    var raycastMouseOver = function (s, e) {
        var domPos = new three_1.Vector2(e.mouseX, e.mouseY);
        var res = doRaycast(mousePosition(s, e));
        processMouseOverObjects(domPos, res);
    };
    var scheduler = helper_1.defScheduler();
    var sink = sink_1.mkSink();
    var disposeTap = core_1.snapshot(raycastTap, size, input.tapped).run(sink, scheduler);
    var disposeMO = core_1.snapshot(raycastMouseOver, size, input.mouseMove).run(sink, scheduler);
    // raycast drag events
    var raycastDrag = function (s, e) {
        var results = doRaycast(dragPosition(s, e));
        return processDragObjects(results, e);
    };
    var unraycastedDrag = core_1.snapshot(raycastDrag, size, input.dragged);
    var disposeDrag = unraycastedDrag.run(sink, scheduler);
    return {
        dragEvent: core_1.multicast(helper_1.unwrap(unraycastedDrag)),
        disposable: disposable_1.disposeAll([disposeTap, disposeMO, disposeDrag])
    };
}
exports.setupRaycasting = setupRaycasting;
//# sourceMappingURL=sceneevent.js.map