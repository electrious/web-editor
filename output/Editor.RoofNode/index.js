// Generated by purs version 0.13.6
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Custom_Mesh = require("../Custom.Mesh/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Editor_RoofEditor = require("../Editor.RoofEditor/index.js");
var Effect = require("../Effect/index.js");
var Effect_Timer = require("../Effect.Timer/index.js");
var Effect_Unsafe = require("../Effect.Unsafe/index.js");
var FRP_Event = require("../FRP.Event/index.js");
var FRP_Event_Class = require("../FRP.Event.Class/index.js");
var Math_Angle = require("../Math.Angle/index.js");
var Models_RoofPlate = require("../Models.RoofPlate/index.js");
var SimplePolygon = require("../SimplePolygon/index.js");
var Three_Core_Geometry = require("../Three.Core.Geometry/index.js");
var Three_Core_Material = require("../Three.Core.Material/index.js");
var Three_Core_Mesh = require("../Three.Core.Mesh/index.js");
var Three_Core_Object3D = require("../Three.Core.Object3D/index.js");
var Three_Math_Vector = require("../Three.Math.Vector/index.js");
var Util = require("../Util/index.js");
var updateRoofPlate = function (v) {
    return function (roof) {
        if (v.length === 0) {
            return roof;
        };
        var newPs = Data_Maybe.fromMaybe(roof.borderPoints)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Array.snoc(v))(Data_Array.head(v)));
        return {
            id: roof.id,
            intId: roof.intId,
            leadId: roof.leadId,
            borderPoints: newPs,
            coefs: roof.coefs,
            center: roof.center,
            normal: roof.normal,
            orientation: roof.orientation,
            alignment: roof.alignment,
            slope: roof.slope,
            azimuth: roof.azimuth,
            rotation: roof.rotation
        };
    };
};
var testSimplePolygon = function (ps) {
    var f = function (p) {
        return [ Three_Math_Vector.vecX(Three_Math_Vector.hasXVec2)(p), Three_Math_Vector.vecY(Three_Math_Vector.hasYVec2)(p) ];
    };
    return SimplePolygon.isSimplePolygon(Data_Functor.map(Data_Functor.functorArray)(f)(ps));
};
var defMaterial = Effect_Unsafe.unsafePerformEffect(function __do() {
    var mat = Three_Core_Material.mkMeshBasicMaterial(16777147)();
    Three_Core_Material.setTransparent(true)(mat)();
    Three_Core_Material.setOpacity(0.7)(mat)();
    return mat;
});
var activeMaterial = Effect_Unsafe.unsafePerformEffect(function __do() {
    var mat = Three_Core_Material.mkMeshBasicMaterial(16777096)();
    Three_Core_Material.setTransparent(true)(mat)();
    Three_Core_Material.setOpacity(0.9)(mat)();
    return mat;
});
var getMaterial = function (v) {
    if (v) {
        return activeMaterial;
    };
    if (!v) {
        return defMaterial;
    };
    throw new Error("Failed pattern match at Editor.RoofNode (line 53, column 1 - line 53, column 47): " + [ v.constructor.name ]);
};
var createRoofMesh = function (ps) {
    return function (active) {
        return function __do() {
            var shp = Three_Core_Geometry.mkShape(ps)();
            var geo = Three_Core_Geometry.mkShapeGeometry(shp)();
            return Custom_Mesh.mkTappableMesh(geo)(getMaterial(active))();
        };
    };
};
var createRoofNode = function (roof) {
    return function (isActive) {
        return function __do() {
            var obj = Three_Core_Object3D.mkObject3D();
            Three_Core_Object3D.setName("roofplate")(obj)();
            Three_Core_Object3D.setPosition(Three_Math_Vector.mkVec3(Three_Math_Vector.vecX(Three_Math_Vector.hasXVec3)(roof.center))(Three_Math_Vector.vecY(Three_Math_Vector.hasYVec3)(roof.center))(Three_Math_Vector.vecZ(Three_Math_Vector.hasZVec3)(roof.center) + 5.0e-2))(obj)();
            Three_Core_Object3D.rotateZ(-Math_Angle.radianVal(roof.azimuth))(obj)();
            Three_Core_Object3D.rotateX(-Math_Angle.radianVal(roof.slope))(obj)();
            Three_Core_Object3D.updateMatrix(obj)();
            Three_Core_Object3D.updateMatrixWorld(obj)();
            var toLocal = function (p) {
                return function __do() {
                    var np = Three_Core_Object3D.worldToLocal(p)(obj)();
                    return Three_Math_Vector.mkVec2(Three_Math_Vector.vecX(Three_Math_Vector.hasXVec3)(np))(Three_Math_Vector.vecY(Three_Math_Vector.hasYVec3)(np));
                };
            };
            var ps = Data_Traversable.traverse(Data_Traversable.traversableArray)(Effect.applicativeEffect)(toLocal)(Data_Maybe.fromMaybe([  ])(Data_Array.init(roof.borderPoints)))();
            var v = FRP_Event.create();
            var editor = Editor_RoofEditor.createRoofEditor(obj)(isActive)(ps)();
            var vertices = Control_Alt.alt(FRP_Event.altEvent)(v.event)(editor.roofVertices);
            var meshEvt = Util.performEvent(Control_Apply.lift2(FRP_Event.applyEvent)(createRoofMesh)(vertices)(isActive));
            var d1 = FRP_Event.subscribe(FRP_Event_Class.withLast(FRP_Event.eventIsEvent)(meshEvt))(function (v1) {
                return function __do() {
                    Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(function (o) {
                        return Three_Core_Object3D.remove(o.mesh)(obj);
                    })(v1.last)();
                    return Three_Core_Object3D.add(v1.now.mesh)(obj)();
                };
            })();
            var v1 = Util.performEvent(Control_Apply.lift2(FRP_Event.applyEvent)(function (m) {
                return function (a) {
                    return Three_Core_Mesh.setMaterial(getMaterial(a))(m.mesh);
                };
            })(meshEvt)(isActive));
            var toParent = function (v2) {
                return Three_Math_Vector.applyMatrix(Three_Core_Object3D.matrix(obj))(Three_Math_Vector.mkVec3(Three_Math_Vector.vecX(Three_Math_Vector.hasXVec2)(v2))(Three_Math_Vector.vecY(Three_Math_Vector.hasYVec2)(v2))(0.0));
            };
            var tapped = FRP_Event_Class.keepLatest(FRP_Event.eventIsEvent)(Data_Functor.map(FRP_Event.functorEvent)(function (m) {
                return m.tapped;
            })(meshEvt));
            var newRoofs = Data_Functor.map(FRP_Event.functorEvent)((function () {
                var $19 = Data_Function.flip(updateRoofPlate)(roof);
                var $20 = Data_Functor.map(Data_Functor.functorArray)(toParent);
                return function ($21) {
                    return Models_RoofPlate.RoofOpUpdate.create($19($20($21)));
                };
            })())(editor.roofVertices);
            var v2 = FRP_Event.create();
            var delRoofEvt = Control_Alt.alt(FRP_Event.altEvent)(v2.event)(Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](Data_Unit.unit))(editor.deleteRoof));
            v.push(ps)();
            Control_Applicative.when(Effect.applicativeEffect)(!testSimplePolygon(ps))(Data_Functor["void"](Effect.functorEffect)(Effect_Timer.setTimeout(1000)(v2.push(Data_Unit.unit))))();
            return {
                roofId: roof.id,
                roofDelete: Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](new Models_RoofPlate.RoofOpDelete(roof.id)))(delRoofEvt),
                roofUpdate: Util.multicast(newRoofs),
                tapped: tapped,
                roofObject: obj,
                disposable: Data_Foldable.sequence_(Effect.applicativeEffect)(Data_Foldable.foldableArray)([ d1, editor.disposable ])
            };
        };
    };
};
module.exports = {
    defMaterial: defMaterial,
    activeMaterial: activeMaterial,
    getMaterial: getMaterial,
    createRoofMesh: createRoofMesh,
    updateRoofPlate: updateRoofPlate,
    testSimplePolygon: testSimplePolygon,
    createRoofNode: createRoofNode
};
