// Generated by purs version 0.13.6
"use strict";
var Algorithm_MeshFlatten = require("../Algorithm.MeshFlatten/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Compactable = require("../Data.Compactable/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Editor_SceneEvent = require("../Editor.SceneEvent/index.js");
var Effect = require("../Effect/index.js");
var FRP_Event = require("../FRP.Event/index.js");
var Three_Core_Geometry = require("../Three.Core.Geometry/index.js");
var Three_Core_Material = require("../Three.Core.Material/index.js");
var Three_Core_Mesh = require("../Three.Core.Mesh/index.js");
var Three_Core_Object3D = require("../Three.Core.Object3D/index.js");
var Three_Loader_ObjLoader = require("../Three.Loader.ObjLoader/index.js");
var Three_Math_Vector = require("../Three.Math.Vector/index.js");
var mkHouseMesh = function (geo) {
    return function (mat) {
        return function __do() {
            var mesh = Three_Core_Mesh.mkMesh(geo)(mat)();
            Three_Core_Object3D.setName("house-mesh")(mesh)();
            var tapEvt = FRP_Event.makeEvent(function (k) {
                return function __do() {
                    Editor_SceneEvent.makeTappable(mesh)(k)();
                    return Editor_SceneEvent.stopTappable(mesh);
                };
            });
            var mouseEvt = FRP_Event.makeEvent(function (k) {
                return function __do() {
                    Editor_SceneEvent.makeMouseMove(mesh)(k)();
                    return Editor_SceneEvent.stopMouseMove(mesh);
                };
            });
            return {
                mesh: mesh,
                tapped: tapEvt,
                mouseMove: mouseEvt
            };
        };
    };
};
var meshPath = function (serverUrl) {
    return function (leadId) {
        return serverUrl + ("/leads/" + (Data_Show.show(Data_Show.showInt)(leadId) + "/mesh/"));
    };
};
var getGeometryInfo = function (mesh) {
    var vecAt = function (attr) {
        return function (i) {
            return Three_Math_Vector.mkVec3(Three_Core_Geometry.getX(i)(attr))(Three_Core_Geometry.getY(i)(attr))(Three_Core_Geometry.getZ(i)(attr));
        };
    };
    var g = Three_Core_Mesh.bufferGeometry(mesh);
    var normAttr = Three_Core_Geometry.getAttribute("normal")(g);
    var normVecs = Data_Functor.map(Data_Functor.functorArray)(vecAt(normAttr))(Data_Array.range(0)(Three_Core_Geometry.count(normAttr) - 1 | 0));
    var posAttr = Three_Core_Geometry.getAttribute("position")(g);
    var posVecs = Data_Functor.map(Data_Functor.functorArray)(vecAt(posAttr))(Data_Array.range(0)(Three_Core_Geometry.count(posAttr) - 1 | 0));
    return {
        geometry: g,
        vertices: posVecs,
        normals: normVecs
    };
};
var getHouseMeshData = function (obj) {
    return function (houseMesh) {
        var d = getGeometryInfo(houseMesh.mesh);
        return function __do() {
            var tree = Algorithm_MeshFlatten.buildRTree(d.vertices)(d.normals)();
            return {
                wrapper: obj,
                mesh: houseMesh,
                geometry: d.geometry,
                verticeTree: tree
            };
        };
    };
};
var applyMaterialCreator = function (matCreator) {
    return function (obj) {
        var mat = Three_Core_Material.getMaterial("scene")(matCreator);
        return function __do() {
            Three_Core_Material.setTransparent(false)(mat)();
            var upd = function (old) {
                return function __do() {
                    Three_Core_Object3D.remove(old)(obj)();
                    var newMesh = mkHouseMesh(Three_Core_Mesh.geometry(old))(mat)();
                    Three_Core_Object3D.add(newMesh.mesh)(obj)();
                    return newMesh;
                };
            };
            var oldMesh = Data_Foldable.find(Data_Foldable.foldableArray)(Three_Core_Mesh.isMesh)(Three_Core_Object3D.children(obj));
            return Data_Traversable.traverse(Data_Traversable.traversableMaybe)(Effect.applicativeEffect)(upd)(oldMesh)();
        };
    };
};
var loadHouse = function (serverUrl) {
    return function (leadId) {
        return function __do() {
            var objLoader = Three_Loader_ObjLoader.makeOBJLoader2();
            var mtlLoader = Three_Loader_ObjLoader.makeMTLLoader();
            var path = meshPath(serverUrl)(leadId);
            var setupShadow = function (o) {
                return function __do() {
                    Three_Core_Object3D.setCastShadow(true)(o)();
                    return Three_Core_Object3D.setReceiveShadow(true)(o)();
                };
            };
            return Data_Compactable.compact(FRP_Event.compactableEvent)(FRP_Event.makeEvent(function (k) {
                return function __do() {
                    Three_Loader_ObjLoader.setPath(path)(mtlLoader)();
                    Three_Loader_ObjLoader.loadMTL(mtlLoader)("scene.mtl")(function (materials) {
                        return function __do() {
                            Three_Core_Material.preload(materials)();
                            return Three_Loader_ObjLoader.loadOBJ(objLoader)(path + "scene.obj")(function (obj) {
                                return function __do() {
                                    Three_Core_Object3D.setName("house-mesh-wrapper")(obj)();
                                    Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableArray)(setupShadow)(Three_Core_Object3D.children(obj))();
                                    var houseMesh = applyMaterialCreator(materials)(obj)();
                                    var meshData = Data_Traversable.traverse(Data_Traversable.traversableMaybe)(Effect.applicativeEffect)(getHouseMeshData(obj))(houseMesh)();
                                    return k(meshData)();
                                };
                            })();
                        };
                    })();
                    return Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit);
                };
            }));
        };
    };
};
module.exports = {
    meshPath: meshPath,
    mkHouseMesh: mkHouseMesh,
    applyMaterialCreator: applyMaterialCreator,
    loadHouse: loadHouse,
    getGeometryInfo: getGeometryInfo,
    getHouseMeshData: getHouseMeshData
};
