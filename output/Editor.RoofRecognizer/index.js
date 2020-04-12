// Generated by purs version 0.13.6
"use strict";
var Algorithm_RoofCheck = require("../Algorithm.RoofCheck/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Custom_Mesh = require("../Custom.Mesh/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Compactable = require("../Data.Compactable/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Effect = require("../Effect/index.js");
var Effect_Unsafe = require("../Effect.Unsafe/index.js");
var FRP_Event = require("../FRP.Event/index.js");
var FRP_Event_Class = require("../FRP.Event.Class/index.js");
var Models_RoofPlate = require("../Models.RoofPlate/index.js");
var Three_Core_Face3 = require("../Three.Core.Face3/index.js");
var Three_Core_Geometry = require("../Three.Core.Geometry/index.js");
var Three_Core_Material = require("../Three.Core.Material/index.js");
var Three_Core_Object3D = require("../Three.Core.Object3D/index.js");
var Three_Math_Vector = require("../Three.Math.Vector/index.js");
var Util = require("../Util/index.js");
var showMarker = function (marker) {
    return function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Three_Core_Object3D.setVisible(false)(marker.mesh);
        };
        if (v instanceof Data_Maybe.Just) {
            if (!Three_Core_Object3D.hasParent(marker.mesh)) {
                return Three_Core_Object3D.setVisible(false)(marker.mesh);
            };
            if (Data_Boolean.otherwise) {
                return function __do() {
                    Three_Core_Object3D.setVisible(true)(marker.mesh)();
                    var np = Three_Math_Vector.addScaled(Three_Math_Vector.vecVec3)(v.value0.position)(v.value0.faceNormal)(3.0e-2);
                    Three_Core_Object3D.setPosition(np)(marker.mesh)();
                    var target = Three_Math_Vector.add(Three_Math_Vector.vecVec3)(v.value0.position)(v.value0.faceNormal);
                    var targetW = Three_Core_Object3D.localToWorld(target)(Three_Core_Object3D.parent(marker.mesh))();
                    return Three_Core_Object3D.lookAt(targetW)(marker.mesh)();
                };
            };
        };
        throw new Error("Failed pattern match at Editor.RoofRecognizer (line 51, column 1 - line 51, column 78): " + [ marker.constructor.name, v.constructor.name ]);
    };
};
var adderMarkerMat = Effect_Unsafe.unsafePerformEffect(Three_Core_Material.mkMeshBasicMaterial(2237183));
var adderMarkerGeo = Effect_Unsafe.unsafePerformEffect(Three_Core_Geometry.mkCircleGeometry(1.0)(32));
var createAdderMarker = function __do() {
    var marker = Custom_Mesh.mkTappableMesh(adderMarkerGeo)(adderMarkerMat)();
    Three_Core_Object3D.setName("add-roof-marker")(marker.mesh)();
    return marker;
};
var createRoofRecognizer = function (houseWrapper) {
    return function (roofs) {
        return function (mouseMove) {
            return function (canShow) {
                return function __do() {
                    var marker = createAdderMarker();
                    Three_Core_Object3D.setVisible(false)(marker.mesh)();
                    var pointCanShow = function (v) {
                        return function (v1) {
                            if (v) {
                                return v1;
                            };
                            if (!v) {
                                return Data_Maybe.Nothing.value;
                            };
                            throw new Error("Failed pattern match at Editor.RoofRecognizer (line 93, column 9 - line 93, column 32): " + [ v.constructor.name, v1.constructor.name ]);
                        };
                    };
                    var mkRoof = function (v) {
                        return function (p) {
                            return Models_RoofPlate.newRoofPlate(p.position)(p.faceNormal);
                        };
                    };
                    var getCandidatePoint = function (evt) {
                        return function (rs) {
                            return function __do() {
                                var isRoof = Algorithm_RoofCheck.couldBeRoof(houseWrapper)(rs)(evt)();
                                if (isRoof) {
                                    var np = Three_Core_Object3D.worldToLocal(evt.point)(houseWrapper)();
                                    return new Data_Maybe.Just({
                                        position: np,
                                        faceNormal: Three_Core_Face3.normal(evt.face)
                                    });
                                };
                                return Data_Maybe.Nothing.value;
                            };
                        };
                    };
                    var point = Util.performEvent(FRP_Event_Class.sampleOn(FRP_Event.eventIsEvent)(roofs)(Data_Functor.map(FRP_Event.functorEvent)(getCandidatePoint)(FRP_Event_Class.gate(FRP_Event.eventIsEvent)(canShow)(mouseMove))));
                    var d = FRP_Event.subscribe(Control_Apply.lift2(FRP_Event.applyEvent)(pointCanShow)(canShow)(point))(showMarker(marker))();
                    var roof = Data_Compactable.compact(FRP_Event.compactableEvent)(Util.performEvent(FRP_Event_Class.sampleOn(FRP_Event.eventIsEvent)(point)(Data_Functor.map(FRP_Event.functorEvent)((function () {
                        var $10 = Data_Traversable.traverse(Data_Traversable.traversableMaybe)(Effect.applicativeEffect);
                        return function ($11) {
                            return $10(mkRoof($11));
                        };
                    })())(marker.tapped))));
                    return {
                        marker: marker.mesh,
                        addedNewRoof: Util.multicast(roof),
                        disposable: d
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
