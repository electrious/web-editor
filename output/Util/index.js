"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Filterable = require("../Data.Filterable/index.js");
var Data_Foreign_EasyFFI = require("../Data.Foreign.EasyFFI/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Int = require("../Data.Int/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect = require("../Effect/index.js");
var Effect_Console = require("../Effect.Console/index.js");
var Effect_Now = require("../Effect.Now/index.js");
var Effect_Ref = require("../Effect.Ref/index.js");
var Effect_Timer = require("../Effect.Timer/index.js");
var Effect_Unsafe = require("../Effect.Unsafe/index.js");
var FRP_Event = require("../FRP.Event/index.js");
var FRP_Event_Class = require("../FRP.Event.Class/index.js");

// | skip first n occurrences of the event
var skip = function (n) {
    return function (evt) {
        var c = FRP_Event_Class.count(FRP_Event.eventIsEvent)(evt);
        var skipped = Data_Functor.map(FRP_Event.functorEvent)(Data_Ord.greaterThan(Data_Ord.ordInt)(n))(c);
        return FRP_Event_Class.gate(FRP_Event.eventIsEvent)(skipped)(evt);
    };
};
var multicast = function (evt) {
    return Effect_Unsafe.unsafePerformEffect(function __do() {
        var v = FRP_Event.create();
        var dispose = FRP_Event.subscribe(evt)(v.push)();
        return v.event;
    });
};

// | Perform events with actions inside.
var performEvent = function (evt) {
    return multicast(FRP_Event.makeEvent(function (k) {
        return FRP_Event.subscribe(evt)(function (act) {
            return Control_Bind.bind(Effect.bindEffect)(act)(k);
        });
    }));
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
var debugWith = function (f) {
    return function (evt) {
        var g = function (v) {
            return Control_Apply.applySecond(Effect.applyEffect)(f(v))(Control_Applicative.pure(Effect.applicativeEffect)(v));
        };
        return performEvent(Data_Functor.map(FRP_Event.functorEvent)(g)(evt));
    };
};
var debug = function (dictShow) {
    return function (evt) {
        var f = function (v) {
            return Control_Apply.applySecond(Effect.applyEffect)(Effect_Console.logShow(dictShow)(v))(Control_Applicative.pure(Effect.applicativeEffect)(v));
        };
        return performEvent(Data_Functor.map(FRP_Event.functorEvent)(f)(evt));
    };
};
var debounce = function (v) {
    return function (evt) {
        return Effect_Unsafe.unsafePerformEffect(function __do() {
            var v1 = FRP_Event.create();
            var timer = Effect_Ref["new"](Data_Maybe.Nothing.value)();
            var timerFunc = function (v2) {
                return function __do() {
                    Effect_Ref.modify(Data_Function["const"](Data_Maybe.Nothing.value))(timer)();
                    return v1.push(v2)();
                };
            };
            var dispose = FRP_Event.subscribe(evt)(function (e) {
                return function __do() {
                    var tf = Effect_Ref.read(timer)();
                    (function () {
                        if (tf instanceof Data_Maybe.Just) {
                            return Effect_Timer.clearTimeout(tf.value0)();
                        };
                        if (tf instanceof Data_Maybe.Nothing) {
                            return Data_Unit.unit;
                        };
                        throw new Error("Failed pattern match at Util (line 51, column 9 - line 53, column 33): " + [ tf.constructor.name ]);
                    })();
                    var newT = Effect_Timer.setTimeout(Data_Maybe.fromMaybe(0)(Data_Int.fromNumber(v)))(timerFunc(e))();
                    return Effect_Ref.write(new Data_Maybe.Just(newT))(timer)();
                };
            })();
            return v1.event;
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
    debounce: debounce,
    skip: skip,
    distinct: distinct,
    performEvent: performEvent,
    foldEffect: foldEffect,
    multicast: multicast,
    debug: debug,
    debugWith: debugWith
};
