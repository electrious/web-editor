"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Custom_Mesh = require("../Custom.Mesh/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Compactable = require("../Data.Compactable/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Int = require("../Data.Int/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Effect = require("../Effect/index.js");
var Effect_Unsafe = require("../Effect.Unsafe/index.js");
var FRP_Event = require("../FRP.Event/index.js");
var FRP_Event_Class = require("../FRP.Event.Class/index.js");
var Three_Core_Geometry = require("../Three.Core.Geometry/index.js");
var Three_Core_Material = require("../Three.Core.Material/index.js");
var Three_Core_Object3D = require("../Three.Core.Object3D/index.js");
var Three_Math_Vector = require("../Three.Math.Vector/index.js");
var UI_DraggableObject = require("../UI.DraggableObject/index.js");
var Util = require("../Util/index.js");

// | calculate the center based on roof vertices
var verticesCenter = function (v) {
    if (v.length === 0) {
        return Three_Math_Vector.mkVec3(0.0)(0.0)(1.0e-2);
    };
    var ty = Data_Foldable.sum(Data_Foldable.foldableArray)(Data_Semiring.semiringNumber)(Data_Functor.map(Data_Functor.functorArray)(Three_Math_Vector.vecY(Three_Math_Vector.hasYVec2))(v));
    var tx = Data_Foldable.sum(Data_Foldable.foldableArray)(Data_Semiring.semiringNumber)(Data_Functor.map(Data_Functor.functorArray)(Three_Math_Vector.vecX(Three_Math_Vector.hasXVec2))(v));
    var l = Data_Int.toNumber(Data_Array.length(v));
    return Three_Math_Vector.mkVec3(tx / l)(ty / l)(1.0e-2);
};
var updatePos = function (o) {
    return function (p) {
        return function __do() {
            Three_Core_Object3D.setPosition(Three_Math_Vector.mkVec3(Three_Math_Vector.vecX(Three_Math_Vector.hasXVec2)(p.position))(Three_Math_Vector.vecY(Three_Math_Vector.hasYVec2)(p.position))(1.0e-2))(o.mesh.mesh)();
            return {
                mesh: o.mesh,
                point: p
            };
        };
    };
};
var toVec2 = function (v) {
    return Three_Math_Vector.mkVec2(Three_Math_Vector.vecX(Three_Math_Vector.hasXVec3)(v))(Three_Math_Vector.vecY(Three_Math_Vector.hasYVec3)(v));
};
var setActive = function (ms) {
    return function (active) {
        return Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableArray)(function (m) {
            return Three_Core_Object3D.setVisible(active)(m.mesh.mesh);
        })(ms);
    };
};
var roofDeleteMaterial = Effect_Unsafe.unsafePerformEffect(Three_Core_Material.mkMeshBasicMaterial(16755234));
var roofDeleteGeometry = Effect_Unsafe.unsafePerformEffect(Three_Core_Geometry.mkCircleGeometry(0.6)(32));

// | create red markers for vertices
var mkRedMarkers = function (roofActive) {
    return function (activeMarker) {
        return function (ps) {
            var psIdx = Data_Array.zip(ps)(Data_Array.range(0)(Data_Array.length(ps) - 1 | 0));
            var mkMarker = function (v) {
                var f = function (act) {
                    return function (v1) {
                        if (v1 instanceof Data_Maybe.Nothing) {
                            return act;
                        };
                        if (v1 instanceof Data_Maybe.Just) {
                            return act && v1.value0 === v.value1;
                        };
                        throw new Error("Failed pattern match at Editor.RoofEditor (line 40, column 19 - line 40, column 38): " + [ act.constructor.name, v1.constructor.name ]);
                    };
                };
                var isActive = Util.multicast(Control_Apply.lift2(FRP_Event.applyEvent)(f)(roofActive)(activeMarker));
                return UI_DraggableObject.createDraggableObject(isActive)(v.value1)(v.value0)(Data_Maybe.Nothing.value)(Data_Maybe.Nothing.value);
            };
            return Data_Traversable.traverse(Data_Traversable.traversableArray)(Effect.applicativeEffect)(mkMarker)(psIdx);
        };
    };
};

// | create material and geometry for the green marker.
var greenMaterial = Effect_Unsafe.unsafePerformEffect(Three_Core_Material.mkMeshBasicMaterial(2293538));

// | given a list of vertices position, calculate all middle points
var greenMarkerPositions = function (v) {
    if (v.length === 0) {
        return [  ];
    };
    if (v.length === 1) {
        return [  ];
    };
    
    // a new list with the head put to end
var v2List = Data_Maybe.fromMaybe([  ])(Control_Apply.lift2(Data_Maybe.applyMaybe)(Data_Array.snoc)(Data_Array.tail(v))(Data_Array.head(v)));
    var v1List = Data_Array.mapWithIndex(function (i) {
        return function (v1) {
            return new Data_Tuple.Tuple(i, v1);
        };
    })(v);
    var h = function (r) {
        return r.point;
    };
    var g = function (r) {
        return r.dist > 1.0;
    };
    var f = function (v1) {
        return function (v2) {
            var v11 = Data_Tuple.snd(v1);
            var idx = Data_Tuple.fst(v1);
            var point = {
                position: Three_Math_Vector.mkVec2((Three_Math_Vector.vecX(Three_Math_Vector.hasXVec2)(v11) + Three_Math_Vector.vecX(Three_Math_Vector.hasXVec2)(v2)) / 2.0)((Three_Math_Vector.vecY(Three_Math_Vector.hasYVec2)(v11) + Three_Math_Vector.vecY(Three_Math_Vector.hasYVec2)(v2)) / 2.0),
                vertIndex: idx + 1 | 0
            };
            return {
                dist: Three_Math_Vector.dist(Three_Math_Vector.vecVec2)(v11)(v2),
                point: point
            };
        };
    };
    var d = Data_Array.zipWith(f)(v1List)(v2List);
    return Data_Functor.map(Data_Functor.functorArray)(h)(Data_Array.filter(g)(d));
};
var greenGeometry = Effect_Unsafe.unsafePerformEffect(Three_Core_Geometry.mkCircleGeometry(0.3)(32));
var mkGreenMarkerMesh = function (p) {
    return function __do() {
        var m = Custom_Mesh.mkTappableMesh(greenGeometry)(greenMaterial)();
        Three_Core_Object3D.setPosition(Three_Math_Vector.mkVec3(Three_Math_Vector.vecX(Three_Math_Vector.hasXVec2)(p))(Three_Math_Vector.vecY(Three_Math_Vector.hasYVec2)(p))(1.0e-2))(m.mesh)();
        return m;
    };
};
var mkGreenMarker = function (p) {
    return function __do() {
        var m = mkGreenMarkerMesh(p.position)();
        return {
            mesh: m,
            point: p
        };
    };
};

// function to create/delete/update green marker objects based on new
// list of GreenMarkerPoint
var updateMarkers = function (parent) {
    return function (ps) {
        return function (oldObjs) {
            if (Data_Array.length(ps) === Data_Array.length(oldObjs)) {
                return Data_Traversable.sequence(Data_Traversable.traversableArray)(Effect.applicativeEffect)(Data_Array.zipWith(updatePos)(oldObjs)(ps));
            };
            if (Data_Array.length(ps) > Data_Array.length(oldObjs)) {
                return function __do() {
                    var updObjs = Data_Traversable.sequence(Data_Traversable.traversableArray)(Effect.applicativeEffect)(Data_Array.zipWith(updatePos)(oldObjs)(Data_Array.take(Data_Array.length(oldObjs))(ps)))();
                    var newObjs = Data_Traversable.sequence(Data_Traversable.traversableArray)(Effect.applicativeEffect)(Data_Functor.map(Data_Functor.functorArray)(mkGreenMarker)(Data_Array.takeEnd(Data_Array.length(ps) - Data_Array.length(oldObjs) | 0)(ps)))();
                    Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableArray)(function (o) {
                        return Three_Core_Object3D.add(o.mesh.mesh)(parent);
                    })(newObjs)();
                    return Data_Semigroup.append(Data_Semigroup.semigroupArray)(updObjs)(newObjs);
                };
            };
            if (Data_Array.length(ps) < Data_Array.length(oldObjs)) {
                return function __do() {
                    var updObjs = Data_Traversable.sequence(Data_Traversable.traversableArray)(Effect.applicativeEffect)(Data_Array.zipWith(updatePos)(Data_Array.take(Data_Array.length(ps))(oldObjs))(ps))();
                    var delObjs = Data_Array.takeEnd(Data_Array.length(oldObjs) - Data_Array.length(ps) | 0)(oldObjs);
                    Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableArray)(function (o) {
                        return Three_Core_Object3D.remove(o.mesh.mesh)(parent);
                    })(delObjs)();
                    return updObjs;
                };
            };
            if (Data_Boolean.otherwise) {
                return Control_Applicative.pure(Effect.applicativeEffect)(oldObjs);
            };
            throw new Error("Failed pattern match at Editor.RoofEditor (line 135, column 1 - line 135, column 125): " + [ parent.constructor.name, ps.constructor.name, oldObjs.constructor.name ]);
        };
    };
};
var mkGreenMarkers = function (parent) {
    return function (active) {
        return function (vertices) {
            var mPosList = Data_Functor.map(FRP_Event.functorEvent)(greenMarkerPositions)(vertices);
            var markers = Util.multicast(Util.foldEffect(updateMarkers(parent))(mPosList)([  ]));
            var res = Util.performEvent(Control_Apply.lift2(FRP_Event.applyEvent)(setActive)(markers)(active));
            var getTap = function (m) {
                return Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](m.point))(m.mesh.tapped);
            };
            var getTapForAll = function (ms) {
                return Data_Foldable.foldl(Data_Foldable.foldableArray)(Control_Alt.alt(FRP_Event.altEvent))(Control_Plus.empty(FRP_Event.plusEvent))(Data_Functor.map(Data_Functor.functorArray)(getTap)(ms));
            };
            return FRP_Event_Class.keepLatest(FRP_Event.eventIsEvent)(Data_Functor.map(FRP_Event.functorEvent)(getTapForAll)(markers));
        };
    };
};

// | get red markers' active status event
var getRedMarkerActiveStatus = function (ms) {
    var statusForNewMarker = Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](Data_Maybe.Nothing.value))(ms);
    var g = function (idx) {
        return function (m) {
            return Data_Functor.map(FRP_Event.functorEvent)(function (d) {
                if (d) {
                    return new Data_Maybe.Just(idx);
                };
                return Data_Maybe.Nothing.value;
            })(m.isDragging);
        };
    };
    var h = function (objs) {
        return Data_Foldable.foldl(Data_Foldable.foldableArray)(Control_Alt.alt(FRP_Event.altEvent))(Control_Plus.empty(FRP_Event.plusEvent))(Data_Array.mapWithIndex(g)(objs));
    };
    var statusForDragging = FRP_Event_Class.keepLatest(FRP_Event.eventIsEvent)(Data_Functor.map(FRP_Event.functorEvent)(h)(ms));
    return Control_Alt.alt(FRP_Event.altEvent)(statusForDragging)(statusForNewMarker);
};

// get new positions after dragging
var getPosition = function (os) {
    var g = function (p) {
        return [ toVec2(p) ];
    };
    var f = function (o) {
        return Data_Functor.map(FRP_Event.functorEvent)(g)(o.position);
    };
    return Data_Foldable.foldl(Data_Foldable.foldableArray)(Data_Semigroup.append(FRP_Event.semigroupEvent(Data_Semigroup.semigroupArray)))(Control_Plus.empty(FRP_Event.plusEvent))(Data_Functor.map(Data_Functor.functorArray)(f)(os));
};
var getDelEvt = function (os) {
    var f = function (o) {
        return o.tapped;
    };
    return Data_Foldable.foldl(Data_Foldable.foldableArray)(Control_Alt.alt(FRP_Event.altEvent))(Control_Plus.empty(FRP_Event.plusEvent))(Data_Functor.map(Data_Functor.functorArray)(f)(os));
};
var delMarker = function (ps) {
    return function (idx) {
        return Data_Maybe.fromMaybe([  ])(Data_Array.deleteAt(idx)(ps));
    };
};

// | create the roof delete marker button
var createRoofDeleteMarker = Custom_Mesh.mkTappableMesh(roofDeleteGeometry)(roofDeleteMaterial);

// | delete old marker objects and add new ones.
var attachObjs = function (parent) {
    return function (newObjs) {
        return function (objs) {
            return function __do() {
                Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableArray)(function (o) {
                    return Control_Apply.applySecond(Effect.applyEffect)(Three_Core_Object3D.remove(o.object)(parent))(o.disposable);
                })(objs)();
                Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableArray)(function (o) {
                    return Three_Core_Object3D.add(o.object)(parent);
                })(newObjs)();
                return newObjs;
            };
        };
    };
};

// | create roof editor
var createRoofEditor = function (parent) {
    return function (active) {
        return function (ps) {
            return function __do() {
                var v = FRP_Event.create();
                var d1 = FRP_Event.subscribe(active)(v.push)();
                var v1 = FRP_Event.create();
                var v2 = FRP_Event.create();
                var markerObjs = Util.performEvent(Data_Functor.map(FRP_Event.functorEvent)(mkRedMarkers(v.event)(v2.event))(v1.event));
                var markers = Util.multicast(Util.foldEffect(attachObjs(parent))(markerObjs)([  ]));
                
                // event for active red marker
var actMarker = getRedMarkerActiveStatus(markers);
                var d2 = FRP_Event.subscribe(actMarker)(v2.push)();
                var vertsAfterDrag = FRP_Event_Class.keepLatest(FRP_Event.eventIsEvent)(Data_Functor.map(FRP_Event.functorEvent)(getPosition)(markers));
                
                // merge new vertices after dragging and vertices after adding/deleting
var newVertices = Util.multicast(Control_Alt.alt(FRP_Event.altEvent)(v1.event)(vertsAfterDrag));
                var greenActive = Util.multicast(Control_Apply.lift2(FRP_Event.applyEvent)(function (ra) {
                    return function (am) {
                        return ra && Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqInt))(am)(Data_Maybe.Nothing.value);
                    };
                })(v.event)(v2.event));
                
                // create green markers for adding new vertices
var toAddEvt = mkGreenMarkers(parent)(greenActive)(newVertices);
                
                // get delete event of tapping on a marker
var delEvts = FRP_Event_Class.keepLatest(FRP_Event.eventIsEvent)(Data_Functor.map(FRP_Event.functorEvent)(getDelEvt)(markers));
                
                // calculate new vertices after deleting a vertex
var vertsAfterDel = Control_Apply.lift2(FRP_Event.applyEvent)(delMarker)(newVertices)(delEvts);
                var addVert = function (pns) {
                    return function (p) {
                        return Data_Array.insertAt(p.vertIndex)(p.position)(pns);
                    };
                };
                var vertsAfterAdd = Data_Compactable.compact(FRP_Event.compactableEvent)(FRP_Event_Class.sampleOn(FRP_Event.eventIsEvent)(toAddEvt)(Data_Functor.map(FRP_Event.functorEvent)(addVert)(newVertices)));
                var d3 = FRP_Event.subscribe(Control_Alt.alt(FRP_Event.altEvent)(vertsAfterAdd)(vertsAfterDel))(function (vs) {
                    return function __do() {
                        v1.push(vs)();
                        return v.push(true)();
                    };
                })();
                var roofDel = createRoofDeleteMarker();
                Three_Core_Object3D.add(roofDel.mesh)(parent)();
                var d4 = FRP_Event.subscribe(Data_Functor.map(FRP_Event.functorEvent)(verticesCenter)(newVertices))(Data_Function.flip(Three_Core_Object3D.setPosition)(roofDel.mesh))();
                var d5 = FRP_Event.subscribe(v.event)(Data_Function.flip(Three_Core_Object3D.setVisible)(roofDel.mesh))();
                v1.push(ps)();
                v.push(false)();
                return {
                    roofVertices: newVertices,
                    deleteRoof: roofDel.tapped,
                    disposable: Data_Foldable.sequence_(Effect.applicativeEffect)(Data_Foldable.foldableArray)([ d1, d2, d3, d4, d5 ])
                };
            };
        };
    };
};
module.exports = {
    toVec2: toVec2,
    mkRedMarkers: mkRedMarkers,
    getRedMarkerActiveStatus: getRedMarkerActiveStatus,
    attachObjs: attachObjs,
    roofDeleteMaterial: roofDeleteMaterial,
    roofDeleteGeometry: roofDeleteGeometry,
    createRoofDeleteMarker: createRoofDeleteMarker,
    greenMaterial: greenMaterial,
    greenGeometry: greenGeometry,
    mkGreenMarkerMesh: mkGreenMarkerMesh,
    mkGreenMarker: mkGreenMarker,
    greenMarkerPositions: greenMarkerPositions,
    setActive: setActive,
    updatePos: updatePos,
    updateMarkers: updateMarkers,
    mkGreenMarkers: mkGreenMarkers,
    verticesCenter: verticesCenter,
    getPosition: getPosition,
    getDelEvt: getDelEvt,
    delMarker: delMarker,
    createRoofEditor: createRoofEditor
};
