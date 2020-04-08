"use strict";
var Control_Bind = require("../Control.Bind/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Filterable = require("../Data.Filterable/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Foreign_EasyFFI = require("../Data.Foreign.EasyFFI/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Effect = require("../Effect/index.js");
var Effect_Now = require("../Effect.Now/index.js");
var Effect_Ref = require("../Effect.Ref/index.js");
var Effect_Timer = require("../Effect.Timer/index.js");
var Effect_Unsafe = require("../Effect.Unsafe/index.js");
var FRP_Behavior = require("../FRP.Behavior/index.js");
var FRP_Event = require("../FRP.Event/index.js");
var FRP_Event_Class = require("../FRP.Event.Class/index.js");

// | skip first n occurrences of the event
var skip = function (n) {
    return function (evt) {
        var c = FRP_Behavior.step(FRP_Event.eventIsEvent)(0)(FRP_Event_Class.count(FRP_Event.eventIsEvent)(evt));
        var skipped = Data_Functor.map(FRP_Behavior.functorABehavior(FRP_Event.functorEvent))(Data_Ord.greaterThan(Data_Ord.ordInt)(n))(c);
        return FRP_Behavior.gate(FRP_Event.eventIsEvent)(skipped)(evt);
    };
};

// | Perform events with actions inside.
var performEvent = function (evt) {
    return FRP_Event.makeEvent(function (k) {
        return FRP_Event.subscribe(evt)(function (act) {
            return Control_Bind.bind(Effect.bindEffect)(act)(k);
        });
    });
};
var multicast = function (evt) {
    return Effect_Unsafe.unsafePerformEffect(function __do() {
        var v = FRP_Event.create();
        var dispose = FRP_Event.subscribe(evt)(v.push)();
        return FRP_Event.makeEvent(function (k) {
            return function __do() {
                var disposeN = FRP_Event.subscribe(v.event)(k)();
                return Data_Foldable.sequence_(Effect.applicativeEffect)(Data_Foldable.foldableArray)([ dispose, disposeN ]);
            };
        });
    });
};
var fpi = Data_Foreign_EasyFFI.unsafeForeignProcedure;

// | fold events with Effect actions 
var foldEffect = function (act) {
    return function (e) {
        return function (b) {
            return FRP_Event.makeEvent(function (k) {
                return function __do() {
                    var result = Effect_Ref["new"](b)();
                    return FRP_Event.subscribe(e)(function (a) {
                        return function __do() {
                            var curB = Effect_Ref.read(result)();
                            var newB = act(a)(curB)();
                            Effect_Ref.write(newB)(result)();
                            return k(newB)();
                        };
                    })();
                };
            });
        };
    };
};
var ffi = Data_Foreign_EasyFFI.unsafeForeignFunction;
var distinct = function (dictEq) {
    return function (evt) {
        var isDiff = function (v) {
            return Data_Eq.eq(Data_Maybe.eqMaybe(dictEq))(v.last)(new Data_Maybe.Just(v.now));
        };
        var getNow = function (v) {
            return v.now;
        };
        return Data_Functor.map(FRP_Event.functorEvent)(getNow)(Data_Filterable.filter(FRP_Event.filterableEvent)(isDiff)(FRP_Event_Class.withLast(FRP_Event.eventIsEvent)(evt)));
    };
};

// | delay the input event by n milliseconds
// TODO: add code to clear timeouts when cancelled
var delay = function (n) {
    return function (evt) {
        return FRP_Event.makeEvent(function (k) {
            return FRP_Event.subscribe(evt)(function (v) {
                return Effect_Timer.setTimeout(n)(k(v));
            });
        });
    };
};

// | create an Event that will fire in n milliseconds
var after = function (n) {
    return FRP_Event.makeEvent(function (k) {
        return function __do() {
            var id = Effect_Timer.setTimeout(n)(function __do() {
                var time = Effect_Now.now();
                return k(time)();
            })();
            return Effect_Timer.clearTimeout(id);
        };
    });
};
module.exports = {
    ffi: ffi,
    fpi: fpi,
    after: after,
    delay: delay,
    skip: skip,
    distinct: distinct,
    performEvent: performEvent,
    foldEffect: foldEffect,
    multicast: multicast
};
