// Generated by purs version 0.13.6
"use strict";
var Algorithm_RoofCheck = require("../Algorithm.RoofCheck/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Custom_Mesh = require("../Custom.Mesh/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Effect = require("../Effect/index.js");
var Effect_Unsafe = require("../Effect.Unsafe/index.js");
var FRP_Behavior = require("../FRP.Behavior/index.js");
var FRP_Event = require("../FRP.Event/index.js");
var FRP_Event_Class = require("../FRP.Event.Class/index.js");
var Models_RoofPlate = require("../Models.RoofPlate/index.js");
var Three_Core_Face3 = require("../Three.Core.Face3/index.js");
var Three_Core_Geometry = require("../Three.Core.Geometry/index.js");
var Three_Core_Material = require("../Three.Core.Material/index.js");
var Three_Core_Object3D = require("../Three.Core.Object3D/index.js");
var Three_Math_Vector = require("../Three.Math.Vector/index.js");
var Util = require("../Util/index.js");
var showMarker = function (adder) {
    return function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Control_Apply.applySecond(Effect.applyEffect)(Three_Core_Object3D.setVisible(false)(adder.marker.mesh))(Control_Applicative.pure(Effect.applicativeEffect)(adder));
        };
        if (v instanceof Data_Maybe.Just) {
            if (!Three_Core_Object3D.hasParent(adder.marker.mesh)) {
                return Control_Apply.applySecond(Effect.applyEffect)(Three_Core_Object3D.setVisible(false)(adder.marker.mesh))(Control_Applicative.pure(Effect.applicativeEffect)(adder));
            };
            if (Data_Boolean.otherwise) {
                return function __do() {
                    Three_Core_Object3D.setVisible(true)(adder.marker.mesh)();
                    var np = Three_Math_Vector.addScaled(Three_Math_Vector.vecVec3)(v.value0.position)(v.value0.faceNormal)(3.0e-2);
                    Three_Core_Object3D.setPosition(np)(adder.marker.mesh)();
                    var target = Three_Math_Vector.add(Three_Math_Vector.vecVec3)(v.value0.position)(v.value0.faceNormal);
                    var targetW = Three_Core_Object3D.localToWorld(target)(Three_Core_Object3D.parent(adder.marker.mesh))();
                    Three_Core_Object3D.lookAt(targetW)(adder.marker.mesh)();
                    return {
                        marker: adder.marker,
                        position: np,
                        normal: v.value0.faceNormal
                    };
                };
            };
        };
        throw new Error("Failed pattern match at Editor.RoofRecognizer (line 60, column 1 - line 60, column 84): " + [ adder.constructor.name, v.constructor.name ]);
    };
};
var adderMarkerMat = Effect_Unsafe.unsafePerformEffect(Three_Core_Material.mkMeshBasicMaterial(2237183));
var adderMarkerGeo = Effect_Unsafe.unsafePerformEffect(Three_Core_Geometry.mkCircleGeometry(1.0)(32));
var createAdderMarker = function __do() {
    var marker = Custom_Mesh.mkTappableMesh(adderMarkerGeo)(adderMarkerMat)();
    Three_Core_Object3D.setName("add-roof-marker")(marker.mesh)();
    return {
        marker: marker,
        position: Three_Math_Vector.mkVec3(0.0)(0.0)(0.0),
        normal: Three_Math_Vector.mkVec3(0.0)(1.0)(0.0)
    };
};
var createRoofRecognizer = function (houseWrapper) {
    return function (roofs) {
        return function (mouseMove) {
            return function (canShow) {
                return function __do() {
                    var adder = createAdderMarker();
                    Three_Core_Object3D.setVisible(false)(adder.marker.mesh)();
                    var mkRoof = function (a) {
                        return Models_RoofPlate.newRoofPlate(a.position)(a.normal);
                    };
                    var f = function (evt) {
                        return function (rs) {
                            return function __do() {
                                var isRoof = Algorithm_RoofCheck.couldBeRoof(houseWrapper)(rs)(evt)();
                                if (isRoof) {
                                    return new Data_Maybe.Just({
                                        position: evt.point,
                                        faceNormal: Three_Core_Face3.normal(evt.face)
                                    });
                                };
                                return Data_Maybe.Nothing.value;
                            };
                        };
                    };
                    var point = Util.performEvent(FRP_Event_Class.sampleOn(FRP_Event.eventIsEvent)(roofs)(Data_Functor.map(FRP_Event.functorEvent)(f)(mouseMove)));
                    var roof = Data_Functor.map(FRP_Event.functorEvent)(mkRoof)(Util.performEvent(Data_Functor.map(FRP_Event.functorEvent)(showMarker(adder))(FRP_Behavior.gate(FRP_Event.eventIsEvent)(canShow)(point))));
                    return {
                        marker: adder.marker.mesh,
                        addedNewRoof: Util.performEvent(roof)
                    };
                };
            };
        };
    };
};
module.exports = {
    adderMarkerMat: adderMarkerMat,
    adderMarkerGeo: adderMarkerGeo,
    createAdderMarker: createAdderMarker,
    showMarker: showMarker,
    createRoofRecognizer: createRoofRecognizer
};
