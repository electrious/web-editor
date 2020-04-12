// Generated by purs version 0.13.6
"use strict";
var Data_DateTime_Instant = require("../Data.DateTime.Instant/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Time_Duration = require("../Data.Time.Duration/index.js");
var Effect_Now = require("../Effect.Now/index.js");
var Effect_Timer = require("../Effect.Timer/index.js");
var FRP_Event = require("../FRP.Event/index.js");
var FRP_Event_Class = require("../FRP.Event.Class/index.js");
var withTime = function (e) {
    return FRP_Event.makeEvent(function (k) {
        return FRP_Event.subscribe(e)(function (value) {
            return function __do() {
                var time = Effect_Now.now();
                return k({
                    time: time,
                    value: value
                })();
            };
        });
    });
};
var interval = function (n) {
    return FRP_Event.makeEvent(function (k) {
        return function __do() {
            var id = Effect_Timer.setInterval(n)(function __do() {
                var time = Effect_Now.now();
                return k(time)();
            })();
            return Effect_Timer.clearInterval(id);
        };
    });
};
var debounceWith = function (process) {
    return function (event) {
        var stamped = withTime(event);
        return FRP_Event_Class.fix(FRP_Event.eventIsEvent)(function (allowed) {
            var processed = process(allowed);
            var expiries = Data_Functor.map(FRP_Event.functorEvent)(function (v) {
                return Data_Maybe.fromMaybe(v.time)(Data_DateTime_Instant.instant(Data_Semigroup.append(Data_Time_Duration.semigroupMilliseconds)(Data_DateTime_Instant.unInstant(v.time))(v.value)));
            })(withTime(Data_Functor.map(FRP_Event.functorEvent)(function (v) {
                return v.period;
            })(processed)));
            var comparison = function (a) {
                return function (b) {
                    return Data_Maybe.maybe(true)(function (v) {
                        return Data_Ord.lessThan(Data_DateTime_Instant.ordDateTime)(v)(b.time);
                    })(a);
                };
            };
            var unblocked = FRP_Event_Class.gateBy(FRP_Event.eventIsEvent)(comparison)(expiries)(stamped);
            return {
                input: Data_Functor.map(FRP_Event.functorEvent)(function (v) {
                    return v.value;
                })(unblocked),
                output: Data_Functor.map(FRP_Event.functorEvent)(function (v) {
                    return v.value;
                })(processed)
            };
        });
    };
};
var debounce = function (period) {
    return debounceWith(Data_Functor.map(FRP_Event.functorEvent)(function (v) {
        return {
            period: period,
            value: v
        };
    }));
};
module.exports = {
    interval: interval,
    withTime: withTime,
    debounce: debounce,
    debounceWith: debounceWith
};