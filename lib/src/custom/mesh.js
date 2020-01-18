"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var three_1 = require("three");
var sceneevent_1 = require("../sceneevent");
var adapter_1 = require("@most/adapter");
var clone_1 = __importDefault(require("ramda/es/clone"));
var core_1 = require("@most/core");
var helper_1 = require("../helper");
var input_1 = require("../input");
/**
 * Extend Mesh with Tappable and provide the tapEvents stream
 */
var TappableMesh = /** @class */ (function (_super) {
    __extends(TappableMesh, _super);
    function TappableMesh(geo, mat) {
        var _this = _super.call(this, geo, mat) || this;
        var _a = adapter_1.createAdapter(), f = _a[0], s = _a[1];
        _this.tapEvents = s;
        _this.insertTap = f;
        return _this;
    }
    TappableMesh.prototype.tapped = function (event) {
        this.insertTap(event);
    };
    return TappableMesh;
}(three_1.Mesh));
exports.TappableMesh = TappableMesh;
/**
 * process the drag events in the stream to make sure all drag start with
 * dragStart and end with dragEnd.
 * @param evt
 */
function validateDrag(evt) {
    var f = function (canDrag, e) {
        if (sceneevent_1.isDragStart(e)) {
            if (canDrag) {
                // if there's a repeated dragStart, omit it.
                return { seed: true, value: null };
            }
            return { seed: true, value: e };
        }
        else if (canDrag && sceneevent_1.isDrag(e)) {
            // only send drag event downstream if there was a drag start event
            return { seed: true, value: e };
        }
        else if (sceneevent_1.isDragEnd(e)) {
            // if the drag already ended, then omit the new End event.
            if (!canDrag) {
                return { seed: false, value: null };
            }
            // otherwise, stop the dragging, and send the end event
            return { seed: false, value: e };
        }
        else {
            // unknown state, omit the event
            return { seed: false, value: null };
        }
    };
    return helper_1.unwrap(core_1.loop(f, false, sceneevent_1.mkDragEndable(evt)));
}
exports.validateDrag = validateDrag;
/**
 * Calculate local delta distances for all drag events
 * @param evt
 * @param toLocal
 */
function calcDragDelta(evt, toLocal) {
    // default drag event as seed for loop
    var def = {
        type: input_1.DragType.DragStart,
        distance: 0,
        point: new three_1.Vector3(0, 0, 0)
    };
    // convert drag event to use local coordinate system
    var e = helper_1.unwrap(core_1.map(function (d) {
        var p = toLocal(d.point);
        if (p != null) {
            var nd = clone_1.default(d);
            nd.point = p;
            return nd;
        }
        return null;
    }, evt));
    // calculate delta between drag events
    var delta = function (oe, e) {
        if (e.type == input_1.DragType.DragStart) {
            return { seed: e, value: new three_1.Vector3(0, 0, 0) };
        }
        else {
            var delta_1 = clone_1.default(e.point);
            delta_1.addScaledVector(oe.point, -1);
            return { seed: e, value: delta_1 };
        }
    };
    return core_1.loop(delta, def, e);
}
exports.calcDragDelta = calcDragDelta;
/**
 * Extend Mesh with Draggable and provide the dragEvents stream
 */
var DraggableMesh = /** @class */ (function (_super) {
    __extends(DraggableMesh, _super);
    function DraggableMesh(geo, mat) {
        var _this = _super.call(this, geo, mat) || this;
        var _a = adapter_1.createAdapter(), f = _a[0], s = _a[1];
        _this.dragEvents = core_1.multicast(s);
        _this.insertDrag = f;
        _this.dragDelta = core_1.multicast(calcDragDelta(_this.dragEvents, function (v) {
            var obj = _this.parent;
            return obj == null ? null : obj.worldToLocal(v);
        }));
        return _this;
    }
    DraggableMesh.prototype.dragged = function (event) {
        this.insertDrag(event);
    };
    return DraggableMesh;
}(three_1.Mesh));
exports.DraggableMesh = DraggableMesh;
/**
 * Extend Mesh with both Tappable and Draggable and provide the event streams
 */
var TapDragMesh = /** @class */ (function (_super) {
    __extends(TapDragMesh, _super);
    function TapDragMesh(geo, mat) {
        var _this = _super.call(this, geo, mat) || this;
        var _a = adapter_1.createAdapter(), tf = _a[0], ts = _a[1];
        _this.tapEvents = ts;
        _this.insertTap = tf;
        return _this;
    }
    TapDragMesh.prototype.tapped = function (event) {
        this.insertTap(event);
    };
    return TapDragMesh;
}(DraggableMesh));
exports.TapDragMesh = TapDragMesh;
//# sourceMappingURL=mesh.js.map