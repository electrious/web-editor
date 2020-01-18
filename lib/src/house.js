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
var OBJLoader2Parallel_1 = require("three/examples/jsm/loaders/OBJLoader2Parallel");
var MTLLoader_1 = require("three/examples/jsm/loaders/MTLLoader");
var three_1 = require("three");
var core_1 = require("@most/core");
var find_1 = __importDefault(require("ramda/es/find"));
var meshflatten_1 = require("./algorithm/meshflatten");
var adapter_1 = require("@most/adapter");
function meshPath(leadId) {
    return ('https://s3.eu-west-1.amazonaws.com/data.electrious.com/leads/' +
        leadId +
        '/mesh/');
}
/**
 * HouseMesh is a Mesh subclass with support for mouse move event and allow user
 * to add new roofs by moving mouse over the mesh.
 */
var HouseMesh = /** @class */ (function (_super) {
    __extends(HouseMesh, _super);
    function HouseMesh(geo, mat) {
        var _this = _super.call(this, geo, mat) || this;
        _this.name = 'house-mesh';
        _this.canAcceptMouseMove = true;
        // create event stream for the tap event
        var _a = adapter_1.createAdapter(), updTap = _a[0], ts = _a[1];
        _this.tappedEvent = ts;
        _this.tapFunc = updTap;
        // create event stream for the mouse events
        var _b = adapter_1.createAdapter(), upd = _b[0], s = _b[1];
        _this.mouseMoveEvent = s;
        _this.mouseMoved = upd;
        return _this;
    }
    HouseMesh.prototype.enableAddingRoof = function (enabled) {
        this.canAcceptMouseMove = enabled;
    };
    HouseMesh.prototype.tapped = function (event) {
        this.tapFunc(event);
    };
    // method to receive mouse move event from the raycaster.
    HouseMesh.prototype.mouseMove = function (event) {
        if (this.canAcceptMouseMove) {
            this.mouseMoved(event);
        }
    };
    return HouseMesh;
}(three_1.Mesh));
/**
 * Apply the material creator's material to the Mesh, which is a child of
 * the loaded Object3D.
 * @param matCreator
 * @param obj
 */
function applyMaterialCreator(matCreator, obj) {
    var mat = matCreator.materials.scene;
    mat.transparent = false;
    var oldMesh = find_1.default(function (child) { return child instanceof three_1.Mesh; }, obj.children);
    if (oldMesh != undefined) {
        obj.remove(oldMesh);
        var m = oldMesh;
        var newMesh = new HouseMesh(m.geometry, mat);
        obj.add(newMesh);
    }
}
/**
 * Load the house mesh of the specified lead.
 * @param {number} leadId
 */
function loadHouse(leadId) {
    var objLoader = new OBJLoader2Parallel_1.OBJLoader2Parallel();
    var mtlLoader = new MTLLoader_1.MTLLoader();
    var path = meshPath(leadId);
    return core_1.fromPromise(new Promise(function (resolve) {
        mtlLoader.setPath(path);
        mtlLoader.load('scene.mtl', function (materials) {
            materials.preload();
            objLoader.load(path + 'scene.obj', function (object) {
                object.name = 'house-mesh-wrapper';
                var childs = object.children;
                childs.forEach(function (c) {
                    c.castShadow = true;
                    c.receiveShadow = true;
                });
                applyMaterialCreator(materials, object);
                resolve(object);
            });
        });
    }));
}
exports.loadHouse = loadHouse;
/**
 * get the house mesh from the loaded Object3D
 * @param obj
 */
function getHouseMesh(obj) {
    var m = find_1.default(function (m) { return m.name == 'house-mesh'; }, obj.children);
    if (m == undefined)
        return undefined;
    return m;
}
/**
 * get the vertex position and normal vector arrays of a BufferGeometry
 * @param mesh
 */
function getGeometryInfo(mesh) {
    var geo = mesh.geometry;
    if (geo instanceof three_1.BufferGeometry) {
        var posAttr = geo.getAttribute('position');
        var normAttr = geo.getAttribute('normal');
        var posVecs = [];
        for (var i = 0; i < posAttr.count; i++) {
            var x = posAttr.getX(i);
            var y = posAttr.getY(i);
            var z = posAttr.getZ(i);
            posVecs.push(new three_1.Vector3(x, y, z));
        }
        var normVecs = [];
        for (var i = 0; i < normAttr.count; i++) {
            var x = normAttr.getX(i);
            var y = normAttr.getY(i);
            var z = normAttr.getZ(i);
            normVecs.push(new three_1.Vector3(x, y, z));
        }
        return { geometry: geo, vertices: posVecs, normals: normVecs };
    }
    return null;
}
/**
 * get the HouseMeshData for the house object loaded.
 * @param obj
 */
function getHouseMeshData(obj) {
    var mesh = getHouseMesh(obj);
    if (mesh == undefined)
        return null;
    var d = getGeometryInfo(mesh);
    if (d == null)
        return null;
    var tree = meshflatten_1.buildRTree(d.vertices, d.normals);
    return { wrapper: obj, mesh: mesh, geometry: d.geometry, verticeTree: tree };
}
exports.getHouseMeshData = getHouseMeshData;
//# sourceMappingURL=house.js.map