"use strict";
var Algorithm_MeshFlatten = require("../Algorithm.MeshFlatten/index.js");
var Control_Alt = require("../Control.Alt/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Compactable = require("../Data.Compactable/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_List = require("../Data.List/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Editor_RoofNode = require("../Editor.RoofNode/index.js");
var Editor_RoofRecognizer = require("../Editor.RoofRecognizer/index.js");
var Effect = require("../Effect/index.js");
var FRP_Event = require("../FRP.Event/index.js");
var FRP_Event_Class = require("../FRP.Event.Class/index.js");
var Models_RoofPlate = require("../Models.RoofPlate/index.js");
var Three_Core_Object3D = require("../Three.Core.Object3D/index.js");
var Util = require("../Util/index.js");

// | update the managed roof dict with new operation
var updateRoofDict = function (v) {
    return function (rd) {
        if (v instanceof Models_RoofPlate.RoofOpCreate) {
            var roofs = Data_Map_Internal.insert(Data_Ord.ordString)(v.value0.id)(v.value0)(rd.roofs);
            return {
                roofs: roofs,
                roofsToRender: new Data_Maybe.Just(roofs)
            };
        };
        if (v instanceof Models_RoofPlate.RoofOpDelete) {
            var roofs = Data_Map_Internal["delete"](Data_Ord.ordString)(v.value0)(rd.roofs);
            return {
                roofs: roofs,
                roofsToRender: new Data_Maybe.Just(roofs)
            };
        };
        if (v instanceof Models_RoofPlate.RoofOpUpdate) {
            var roofs = Data_Map_Internal.insert(Data_Ord.ordString)(v.value0.id)(v.value0)(rd.roofs);
            return {
                roofs: roofs,
                roofsToRender: Data_Maybe.Nothing.value
            };
        };
        throw new Error("Failed pattern match at Editor.RoofManager (line 44, column 1 - line 44, column 64): " + [ v.constructor.name, rd.constructor.name ]);
    };
};
var roofDict = (function () {
    var f = function (r) {
        return new Data_Tuple.Tuple(r.id, r);
    };
    var $22 = Data_Map_Internal.fromFoldable(Data_Ord.ordString)(Data_Foldable.foldableArray);
    var $23 = Data_Functor.map(Data_Functor.functorArray)(f);
    return function ($24) {
        return $22($23($24));
    };
})();

// | get roofUpdate event from an array of roof nodes
var getRoofUpdate = function (ns) {
    return Data_Foldable.foldl(Data_Foldable.foldableArray)(Control_Alt.alt(FRP_Event.altEvent))(Control_Plus.empty(FRP_Event.plusEvent))(Data_Functor.map(Data_Functor.functorArray)(function (v) {
        return v.roofUpdate;
    })(ns));
};

// | get roofDelete event from an array of roof nodes
var getRoofDelete = function (ns) {
    return Data_Foldable.foldl(Data_Foldable.foldableArray)(Control_Alt.alt(FRP_Event.altEvent))(Control_Plus.empty(FRP_Event.plusEvent))(Data_Functor.map(Data_Functor.functorArray)(function (v) {
        return v.roofDelete;
    })(ns));
};

// | get the activated roof id event from an array of roof nodes
var getActivated = function (ns) {
    var f = function (n) {
        return Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](n.roofId))(n.tapped);
    };
    return Data_Foldable.foldl(Data_Foldable.foldableArray)(Control_Alt.alt(FRP_Event.altEvent))(Control_Plus.empty(FRP_Event.plusEvent))(Data_Functor.map(Data_Functor.functorArray)(f)(ns));
};
var doFlatten = function (meshData) {
    return function (rd) {
        return Algorithm_MeshFlatten.flattenRoofPlates(meshData.geometry)(meshData.verticeTree)(meshData.mesh.mesh)(Data_List.toUnfoldable(Data_Unfoldable.unfoldableArray)(Data_Map_Internal.values(rd)));
    };
};

// | create RoofManager for an array of roofs
var createRoofManager = function (meshData) {
    return function (defRoofs) {
        return function __do() {
            var wrapper = Three_Core_Object3D.mkObject3D();
            Three_Core_Object3D.setName("roof wrapper")(wrapper)();
            var v = FRP_Event.create();
            var d1 = FRP_Event.subscribe(Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](Data_Maybe.Nothing.value))(meshData.mesh.tapped))(v.push)();
            var v1 = FRP_Event.create();
            
            // get roofs to be rerendered
var rsToRender = Data_Compactable.compact(FRP_Event.compactableEvent)(Data_Functor.map(FRP_Event.functorEvent)(function (v2) {
                return v2.roofsToRender;
            })(v1.event));
            var rsToRenderArr = Data_Functor.map(FRP_Event.functorEvent)((function () {
                var $25 = Data_List.toUnfoldable(Data_Unfoldable.unfoldableArray);
                return function ($26) {
                    return $25(Data_Map_Internal.values($26));
                };
            })())(rsToRender);
            var renderNodes = function (v2) {
                return function __do() {
                    Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableArray)(function (o) {
                        return Three_Core_Object3D.remove(o.roofObject)(wrapper);
                    })(Data_Maybe.fromMaybe([  ])(v2.last))();
                    Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableArray)(function (o) {
                        return Three_Core_Object3D.add(o.roofObject)(wrapper);
                    })(v2.now)();
                    return v2.now;
                };
            };
            
            // event of new roofs that will be updated on any change and
            // run the roof flatten algorithm whenever there's new roof change
var newRoofs = Util.multicast(Data_Functor.map(FRP_Event.functorEvent)(function (v2) {
                return v2.roofs;
            })(v1.event));
            var mkNode = function (roof) {
                return Editor_RoofNode.createRoofNode(roof)(Util.multicast(Data_Functor.map(FRP_Event.functorEvent)(Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqString))(new Data_Maybe.Just(roof.id)))(v.event)));
            };
            
            // create roofnode for each roof
var nodes = Util.performEvent(Data_Functor.map(FRP_Event.functorEvent)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Effect.applicativeEffect)(mkNode))(rsToRenderArr));
            var renderedNodes = Util.multicast(Util.performEvent(Data_Functor.map(FRP_Event.functorEvent)(renderNodes)(FRP_Event_Class.withLast(FRP_Event.eventIsEvent)(nodes))));
            var updateRoofOp = FRP_Event_Class.keepLatest(FRP_Event.eventIsEvent)(Data_Functor.map(FRP_Event.functorEvent)(getRoofUpdate)(renderedNodes));
            var flattened = Util.performEvent(Data_Functor.map(FRP_Event.functorEvent)(doFlatten(meshData))(newRoofs));
            var deleteRoofOp = Util.multicast(FRP_Event_Class.keepLatest(FRP_Event.eventIsEvent)(Data_Functor.map(FRP_Event.functorEvent)(getRoofDelete)(renderedNodes)));
            var defRoofDict = roofDict(defRoofs);
            
            // create the roof recognizer and add it to the roof wrapper object
var canShowRecognizer = Data_Functor.map(FRP_Event.functorEvent)(Data_Maybe.isNothing)(v.event);
            var recognizer = Editor_RoofRecognizer.createRoofRecognizer(meshData.wrapper)(Data_Functor.map(FRP_Event.functorEvent)((function () {
                var $27 = Data_List.toUnfoldable(Data_Unfoldable.unfoldableArray);
                return function ($28) {
                    return $27(Data_Map_Internal.values($28));
                };
            })())(newRoofs))(meshData.mesh.mouseMove)(canShowRecognizer)();
            Three_Core_Object3D.add(recognizer.marker)(wrapper)();
            var addRoofOp = Data_Functor.map(FRP_Event.functorEvent)(Models_RoofPlate.RoofOpCreate.create)(recognizer.addedNewRoof);
            var ops = Control_Alt.alt(FRP_Event.altEvent)(Control_Alt.alt(FRP_Event.altEvent)(addRoofOp)(deleteRoofOp))(updateRoofOp);
            var d2 = FRP_Event.subscribe(Data_Functor.map(FRP_Event.functorEvent)(Data_Maybe.Just.create)(FRP_Event_Class.keepLatest(FRP_Event.eventIsEvent)(Data_Functor.map(FRP_Event.functorEvent)(getActivated)(renderedNodes))))(v.push)();
            var d3 = FRP_Event.subscribe(Util.delay(1)(Data_Functor.map(FRP_Event.functorEvent)(Data_Function["const"](Data_Maybe.Nothing.value))(deleteRoofOp)))(v.push)();
            var d4 = FRP_Event.subscribe(Util.delay(1)(Data_Functor.map(FRP_Event.functorEvent)(function (o) {
                return new Data_Maybe.Just(o.id);
            })(recognizer.addedNewRoof)))(v.push)();
            var defRoofData = {
                roofs: defRoofDict,
                roofsToRender: new Data_Maybe.Just(defRoofDict)
            };
            var roofData = FRP_Event_Class.fold(FRP_Event.eventIsEvent)(updateRoofDict)(ops)(defRoofData);
            var d5 = FRP_Event.subscribe(roofData)(v1.push)();
            v1.push(defRoofData)();
            v.push(Data_Maybe.Nothing.value)();
            var getRoofEdited = (function () {
                var $29 = Data_Functor.map(Data_Functor.functorArray)(Models_RoofPlate.toRoofEdited);
                var $30 = Data_List.toUnfoldable(Data_Unfoldable.unfoldableArray);
                return function ($31) {
                    return $29($30(Data_Map_Internal.values($31)));
                };
            })();
            return {
                roofWrapper: wrapper,
                editedRoofs: Util.multicast(Util.skip(1)(Util.debounce(1000.0)(Data_Functor.map(FRP_Event.functorEvent)(getRoofEdited)(newRoofs)))),
                disposable: Data_Foldable.sequence_(Effect.applicativeEffect)(Data_Foldable.foldableArray)([ d1, d2, d3, d4, d5, recognizer.disposable ])
            };
        };
    };
};
module.exports = {
    roofDict: roofDict,
    updateRoofDict: updateRoofDict,
    doFlatten: doFlatten,
    getRoofUpdate: getRoofUpdate,
    getRoofDelete: getRoofDelete,
    getActivated: getActivated,
    createRoofManager: createRoofManager
};
