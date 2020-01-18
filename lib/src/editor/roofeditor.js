"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var three_1 = require("three");
var adapter_1 = require("@most/adapter");
var core_1 = require("@most/core");
var draggableobject_1 = require("../ui/draggableobject");
var sink_1 = require("../sink");
var helper_1 = require("../helper");
var disposable_1 = require("@most/disposable");
var pluck_1 = __importDefault(require("ramda/es/pluck"));
var remove_1 = __importDefault(require("ramda/es/remove"));
var curry_1 = __importDefault(require("ramda/es/curry"));
var memoizeWith_1 = __importDefault(require("ramda/es/memoizeWith"));
var always_1 = __importDefault(require("ramda/es/always"));
var mesh_1 = require("../custom/mesh");
var zipWith_1 = __importDefault(require("ramda/es/zipWith"));
var tail_1 = __importDefault(require("ramda/es/tail"));
var take_1 = __importDefault(require("ramda/es/take"));
var takeLast_1 = __importDefault(require("ramda/es/takeLast"));
var concat_1 = __importDefault(require("ramda/es/concat"));
var filter_1 = __importDefault(require("ramda/es/filter"));
var insert_1 = __importDefault(require("ramda/es/insert"));
var append_1 = __importDefault(require("ramda/es/append"));
var head_1 = __importDefault(require("ramda/es/head"));
var toVec2 = function (v) {
    return new three_1.Vector2(v.x, v.y);
};
var mkArray = function () {
    var args = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        args[_i] = arguments[_i];
    }
    return args.map(toVec2);
};
/**
 * create red markers for vertices
 */
var mkRedMarkers = curry_1.default(function (roofActive, activeMarker, ps) {
    var res = ps.map(function (pos, idx) {
        // determine whether the current marker should be active or not
        var f = function (roofActive, activeIdx) {
            return roofActive && (activeIdx == null || activeIdx == idx);
        };
        var isActive = core_1.multicast(core_1.combine(f, roofActive, activeMarker));
        // create the marker
        return draggableobject_1.createDraggableObject(isActive, idx, pos);
    });
    return res;
});
/**
 * get red markers' active status event stream
 * @param ms a stream of all red markers
 */
var getRedMarkerActiveStatus = function (ms) {
    // get active marker when any one is being dragged
    var g = function (m, idx) {
        return core_1.map(function (d) {
            return d ? idx : null;
        }, m.isDragging);
    };
    var h = function (ms) {
        return core_1.mergeArray(ms.map(g));
    };
    var statusForDragging = core_1.switchLatest(core_1.map(h, ms));
    // set active marker to null when all markers are just created
    var statusForNewMarker = core_1.constant(null, ms);
    return core_1.merge(statusForDragging, statusForNewMarker);
};
// delete old marker objects and add new ones. this will return a function
// that can be used in 'loop' directly
var attachObjs = function (parent) {
    return function (objs, newObjs) {
        objs.forEach(function (o) {
            parent.remove(o.object);
            disposable_1.dispose(o.disposable);
        });
        newObjs.forEach(function (o) {
            parent.add(o.object);
        });
        return { seed: newObjs, value: newObjs };
    };
};
/**
 * create the geometry and material for roof delete marker button. These are all
 * memoized functions to be shared.
 */
var roofDeleteMaterial = memoizeWith_1.default(always_1.default('roof_del_material'), function () {
    return new three_1.MeshBasicMaterial({ color: 0xffaa22 });
});
var roofDeleteGeometry = memoizeWith_1.default(always_1.default('roof_del_geometry'), function () {
    return new three_1.CircleGeometry(0.6, 32);
});
/**
 * create the roof delete marker button
 */
var createRoofDeleteMarker = function () {
    var geo = roofDeleteGeometry();
    var mat = roofDeleteMaterial();
    return new mesh_1.TappableMesh(geo, mat);
};
/**
 * create material for the green marker. this function is memoized so the material
 * is created once and shared.
 */
var greenMaterial = memoizeWith_1.default(always_1.default('green_material'), function () {
    return new three_1.MeshBasicMaterial({ color: 0x22ff22 });
});
var greenGeometry = memoizeWith_1.default(always_1.default('green_geometry'), function () {
    return new three_1.CircleGeometry(0.3, 32);
});
/**
 * create a green marker object
 */
var mkGreenMarker = function (p) {
    var geo = greenGeometry();
    var mat = greenMaterial();
    var m = new mesh_1.TappableMesh(geo, mat);
    m.position.set(p.x, p.y, 0.01);
    return m;
};
/**
 * given a list of vertices position, calculate all middle points
 * @param vertices
 */
var greenMarkerPositions = function (vertices) {
    if (vertices.length <= 1) {
        return [];
    }
    // calculate the distance and marker point for two point
    var f = function (v, v2) {
        var v1 = v[0];
        var idx = v[1];
        var markerPoint = {
            position: new three_1.Vector2((v1.x + v2.x) / 2, (v1.y + v2.y) / 2),
            vertIndex: idx + 1
        };
        return { dist: v1.distanceTo(v2), point: markerPoint };
    };
    // take all vertices and their index
    var v1List = vertices.map(function (v, i) { return [v, i]; });
    // make a new list with the head of origin list put to end
    var headEl = head_1.default(vertices);
    if (headEl == undefined)
        return [];
    var v2List = append_1.default(headEl, tail_1.default(vertices));
    // calculate the distance and points between vertex pairs
    var d = zipWith_1.default(f, v1List, v2List);
    // filter function
    var g = function (d) {
        return d.dist > 1;
    };
    // extract the marker point
    var h = function (d) {
        return d.point;
    };
    return filter_1.default(g, d).map(h);
};
var mkGreenMarkers = function (parent, active, vertices) {
    var mPosLst = core_1.map(greenMarkerPositions, vertices);
    var updatePos = function (o, p) {
        o.mesh.position.set(p.position.x, p.position.y, 0.01);
        o.point = p;
        return o;
    };
    // function to create/delete/update green marker objects based on new list
    // of GreenMarkerPoint
    var f = function (oldObjs, ps) {
        var res = [];
        if (oldObjs.length == ps.length) {
            res = zipWith_1.default(updatePos, oldObjs, ps);
        }
        else if (oldObjs.length < ps.length) {
            var updObjs = zipWith_1.default(updatePos, oldObjs, take_1.default(oldObjs.length, ps));
            var newObjs = takeLast_1.default(ps.length - oldObjs.length, ps).map(function (p) {
                var m = mkGreenMarker(p.position);
                return { mesh: m, point: p };
            });
            newObjs.forEach(function (o) {
                parent.add(o.mesh);
            });
            res = concat_1.default(updObjs, newObjs);
        }
        else if (oldObjs.length > ps.length) {
            var updObjs = zipWith_1.default(updatePos, take_1.default(ps.length, oldObjs), ps);
            var delObjs = takeLast_1.default(oldObjs.length - ps.length, oldObjs);
            delObjs.forEach(function (o) {
                parent.remove(o.mesh);
            });
            res = updObjs;
        }
        return { seed: res, value: res };
    };
    var markers = core_1.multicast(core_1.loop(f, [], mPosLst));
    // set active status for all markers
    var setActive = function (ms, active) {
        ms.forEach(function (m) {
            m.mesh.visible = active;
        });
    };
    var d = core_1.combine(setActive, markers, active).run(sink_1.mkSink(), helper_1.defScheduler());
    // transform tap event to add vertex event
    var getTap = function (m) {
        return core_1.constant(m.point, m.mesh.tapEvents);
    };
    var getTapForAll = function (ms) {
        return core_1.mergeArray(ms.map(getTap));
    };
    var tapEvts = core_1.switchLatest(core_1.map(getTapForAll, markers));
    return [tapEvts, d];
};
/**
 * calculate the center based on roof vertices
 * @param vs
 */
function verticesCenter(vs) {
    if (vs.length == 0)
        return new three_1.Vector3(0, 0, 0.01);
    var x = 0, y = 0;
    vs.forEach(function (v) {
        x += v.x;
        y += v.y;
    });
    x /= vs.length;
    y /= vs.length;
    return new three_1.Vector3(x, y, 0.01);
}
// create roof corner markers
exports.createRoofEditor = function (parent, active, ps) {
    var scheduler = helper_1.defScheduler();
    // internal stream for maintaning the roof active status
    var _a = adapter_1.createAdapter(), setRoofActive = _a[0], roofActive = _a[1];
    // pipe the 'active' param event into internal roofActive stream
    var d1 = active.run(sink_1.mkSink(setRoofActive), scheduler);
    // stream for new list of vertices
    var _b = adapter_1.createAdapter(), updateVertList = _b[0], vertices = _b[1];
    // internal stream for currently active marker
    var _c = adapter_1.createAdapter(), setActive = _c[0], activeMarker = _c[1];
    // create new markers and attach them to the parent object.
    var markerObjs = core_1.map(mkRedMarkers(roofActive, activeMarker), vertices);
    var markers = core_1.multicast(core_1.loop(attachObjs(parent), [], markerObjs));
    // stream for active red marker
    var actMarker = getRedMarkerActiveStatus(markers);
    var d2 = actMarker.run(sink_1.mkSink(setActive), scheduler);
    // get new positions after dragging
    var getPosition = function (o) {
        return core_1.skip(1, core_1.combineArray(mkArray, pluck_1.default('position', o)));
    };
    var vertsAfterDrag = core_1.switchLatest(core_1.map(getPosition, markers));
    // merge new vertices after dragging and vertices after adding/deleting
    var newVertices = core_1.multicast(core_1.merge(vertices, vertsAfterDrag));
    var greenActive = core_1.multicast(core_1.combine(function (ra, am) {
        return ra && am == null;
    }, roofActive, activeMarker));
    // create green markers for adding new vertices
    var _d = mkGreenMarkers(parent, greenActive, newVertices), toAddVert = _d[0], d3 = _d[1];
    var addVert = function (ps, p) {
        return insert_1.default(p.vertIndex, p.position, ps);
    };
    var vertsAfterAdd = core_1.snapshot(addVert, newVertices, toAddVert);
    // get delete event of tapping on a marker
    var getDelEvt = function (o) {
        return core_1.mergeArray(pluck_1.default('tapped', o));
    };
    var delEvts = core_1.switchLatest(core_1.map(getDelEvt, markers));
    // calculate new vertices after deleting a vertex
    var delMarker = function (ps, idx) {
        return remove_1.default(idx, 1, ps);
    };
    var vertsAfterDel = core_1.snapshot(delMarker, newVertices, delEvts);
    // update the real vertex list after adding/deleting
    var d4 = core_1.merge(vertsAfterAdd, vertsAfterDel).run(sink_1.mkSink(function (vs) {
        updateVertList(vs);
        // reset rofoActive to true so that the 'combine' call in
        // mkMarkers func will be able to continue without waiting for
        // explicit event from the 'active' param stream
        setRoofActive(true);
    }), scheduler);
    // create the roof delete button
    var roofDel = createRoofDeleteMarker();
    parent.add(roofDel);
    var d5 = core_1.map(verticesCenter, newVertices).run(sink_1.mkSink(function (p) { return roofDel.position.copy(p); }), scheduler);
    var d6 = roofActive.run(sink_1.mkSink(function (a) {
        roofDel.visible = a;
    }), scheduler);
    // set the default vertices
    updateVertList(ps);
    // set roof to be inactive by default
    setRoofActive(false);
    return {
        // skip the first occurrence as it's the same values as provided
        roofVertices: newVertices,
        deleteRoof: roofDel.tapEvents,
        disposable: disposable_1.disposeAll([d1, d2, d3, d4, d5, d6])
    };
};
//# sourceMappingURL=roofeditor.js.map