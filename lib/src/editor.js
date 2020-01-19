"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var three_1 = require("three");
var house_1 = require("./house");
var sink_1 = require("./sink");
var input_1 = require("./input");
var disposable_1 = require("@most/disposable");
var adapter_1 = require("@most/adapter");
var roofplate_1 = require("./models/roofplate");
var sceneevent_1 = require("./sceneevent");
var helper_1 = require("./helper");
var roofmanager_1 = require("./roofmanager");
var core_1 = require("@most/core");
function zoomCamera(camera, zoom) {
    var pos = camera.position;
    var curDist = pos.length();
    var newDist = curDist + zoom;
    if (newDist < 5) {
        newDist = 5;
    }
    else if (newDist > 500) {
        newDist = 500;
    }
    // normalize the position and multiply the new distance
    pos.normalize();
    pos.multiplyScalar(newDist);
    return newDist;
}
function rotateContentWithDrag(obj, drag) {
    obj.rotateZ(drag.deltaX / 360);
    obj.rotateOnWorldAxis(new three_1.Vector3(1, 0, 0), drag.deltaY / 360);
}
function moveWithShiftDrag(obj, drag, scale) {
    var p = obj.parent;
    if (p == null) {
        return;
    }
    var vec = new three_1.Vector3(drag.deltaX, -drag.deltaY, 0);
    p.worldToLocal(vec);
    obj.translateX((vec.x * scale) / 10);
    obj.translateY((vec.y * scale) / 10);
}
/**
 * internal function to create the Threejs scene, camera, light and renderer.
 * @param width
 * @param height
 * @param elem
 */
function createScene(width, height, elem) {
    var scene = new three_1.Scene();
    // create a disposable for the scene
    var sceneDisposable = disposable_1.disposeWith(function (s) {
        s.dispose();
    }, scene);
    var camera = new three_1.PerspectiveCamera(45, width / height, 0.1, 1000);
    var renderer = new three_1.WebGLRenderer();
    var _a = adapter_1.createAdapter(), updateSize = _a[0], sizeStream = _a[1];
    var scheduler = helper_1.defScheduler();
    // function to update camera and renderer when size changed.
    var resized = function (s) {
        var width = s[0];
        var height = s[1];
        camera.aspect = width / height;
        camera.updateProjectionMatrix();
        renderer.setSize(width, height);
    };
    var disposable0 = sizeStream.run(sink_1.mkSink(resized), scheduler);
    // attach the WebGL canvas to a parent DOM element
    elem.appendChild(renderer.domElement);
    // set the camera position and orient it toward the center
    camera.position.set(0, -50, 50);
    camera.lookAt(0, 0, 0);
    var cameraDefDist = camera.position.length();
    // add ambient light
    var ambLight = new three_1.AmbientLight(0xffffff);
    ambLight.name = 'ambient-light';
    scene.add(ambLight);
    // add a directional light
    var dirLight = new three_1.DirectionalLight(0xeeeeee, 0.5);
    dirLight.name = 'directional-light';
    dirLight.position.set(100, 0, 100);
    scene.add(dirLight);
    // add a wrapper object that's used only to rotate the scene around
    var rotWrapper = new three_1.Object3D();
    rotWrapper.name = 'rotate-wrapper';
    scene.add(rotWrapper);
    // add a wrapper object to be parent all user contents
    var content = new three_1.Object3D();
    content.name = 'scene-content';
    rotWrapper.add(content);
    /**
     * function to update renderring of the WebGL scene
     */
    var renderFunc = function () {
        renderer.render(scene, camera);
    };
    var addContentFunc = function (obj) {
        content.add(obj);
    };
    /**
     * setup input events for the scene
     */
    var scale = 1;
    var inputEvts = input_1.setupInput(elem);
    var disposable1 = inputEvts.zoomed.run(sink_1.mkSink(function (e) {
        var newDist = zoomCamera(camera, e.deltaY);
        scale = newDist / cameraDefDist;
    }), scheduler);
    var rcs = sceneevent_1.setupRaycasting(camera, scene, inputEvts, sizeStream);
    // use drag events filtered by raycasting already
    var disposable2 = rcs.dragEvent.run(sink_1.mkSink(function (e) { return rotateContentWithDrag(rotWrapper, e); }), scheduler);
    // use shift drag events to translate the whole scene
    var disposable3 = inputEvts.shiftDragged.run(sink_1.mkSink(function (e) { return moveWithShiftDrag(content, e, scale); }), scheduler);
    // update with the default size
    updateSize([width, height]);
    return {
        scene: scene,
        camera: camera,
        renderer: renderer,
        size: sizeStream,
        disposable: disposable_1.disposeAll([
            sceneDisposable,
            disposable0,
            disposable1,
            disposable2,
            disposable3,
            rcs.disposable,
            inputEvts.disposable
        ]),
        render: renderFunc,
        resize: function (width, height) {
            updateSize([width, height]);
        },
        addContent: addContentFunc
    };
}
/**
 * createEditor will create the Web Editor instance
 * @param {Number} width width of the viewport
 * @param {Number} height height of the viewport
 * @param {HTMLElement} dom parent DOM element to attach the WebGL canvas to
 */
function createEditor(width, height, elem) {
    // create the editor scene
    var es = createScene(width, height, elem);
    // an array of disposables to be disposed at the end of the editor
    var disposables = [es.disposable];
    var renderLoop = function () {
        requestAnimationFrame(renderLoop);
        es.render();
    };
    // start the renderring
    renderLoop();
    /**
     * dispose all resources used by the editor
     */
    var disposeFunc = function () {
        disposable_1.disposeAll(disposables);
    };
    var loadHouseFunc = function (leadId, roofs, roofUpdated) {
        var loadRoofs = function (md) {
            // add all roofs to a new roof manager
            var mgr = roofmanager_1.createRoofManager(md, roofs.map(roofplate_1.fromJSRoofPlate));
            disposables.push(mgr.disposable);
            disposables.push(core_1.map(roofplate_1.toJSRoofOperation, mgr.roofOps).run(sink_1.mkSink(roofUpdated), helper_1.defScheduler()));
            es.addContent(mgr.roofWrapper);
        };
        var f = function (h) {
            es.addContent(h);
            var md = house_1.getHouseMeshData(h);
            if (md != null)
                loadRoofs(md);
        };
        var disp = house_1.loadHouse(leadId).run(sink_1.mkSink(f), helper_1.defScheduler());
        disposables.push(disp);
    };
    var editor = {
        resize: es.resize,
        dispose: disposeFunc,
        loadHouse: loadHouseFunc
    };
    return editor;
}
exports.createEditor = createEditor;
//# sourceMappingURL=editor.js.map