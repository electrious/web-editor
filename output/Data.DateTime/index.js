// Generated by purs version 0.13.6
"use strict";
var $foreign = require("./foreign.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Date = require("../Data.Date/index.js");
var Data_Date_Component = require("../Data.Date.Component/index.js");
var Data_Enum = require("../Data.Enum/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Time = require("../Data.Time/index.js");
var Data_Time_Component = require("../Data.Time.Component/index.js");
var Data_Time_Duration = require("../Data.Time.Duration/index.js");
var DateTime = (function () {
    function DateTime(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    DateTime.create = function (value0) {
        return function (value1) {
            return new DateTime(value0, value1);
        };
    };
    return DateTime;
})();
var toRecord = function (v) {
    return {
        year: Data_Enum.fromEnum(Data_Date_Component.boundedEnumYear)(Data_Date.year(v.value0)),
        month: Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(Data_Date.month(v.value0)),
        day: Data_Enum.fromEnum(Data_Date_Component.boundedEnumDay)(Data_Date.day(v.value0)),
        hour: Data_Enum.fromEnum(Data_Time_Component.boundedEnumHour)(Data_Time.hour(v.value1)),
        minute: Data_Enum.fromEnum(Data_Time_Component.boundedEnumMinute)(Data_Time.minute(v.value1)),
        second: Data_Enum.fromEnum(Data_Time_Component.boundedEnumSecond)(Data_Time.second(v.value1)),
        millisecond: Data_Enum.fromEnum(Data_Time_Component.boundedEnumMillisecond)(Data_Time.millisecond(v.value1))
    };
};
var time = function (v) {
    return v.value1;
};
var showDateTime = new Data_Show.Show(function (v) {
    return "(DateTime " + (Data_Show.show(Data_Date.showDate)(v.value0) + (" " + (Data_Show.show(Data_Time.showTime)(v.value1) + ")")));
});
var modifyTimeF = function (dictFunctor) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictFunctor)(DateTime.create(v.value0))(f(v.value1));
        };
    };
};
var modifyTime = function (f) {
    return function (v) {
        return new DateTime(v.value0, f(v.value1));
    };
};
var modifyDateF = function (dictFunctor) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictFunctor)(Data_Function.flip(DateTime.create)(v.value1))(f(v.value0));
        };
    };
};
var modifyDate = function (f) {
    return function (v) {
        return new DateTime(f(v.value0), v.value1);
    };
};
var eqDateTime = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(Data_Date.eqDate)(x.value0)(y.value0) && Data_Eq.eq(Data_Time.eqTime)(x.value1)(y.value1);
    };
});
var ordDateTime = new Data_Ord.Ord(function () {
    return eqDateTime;
}, function (x) {
    return function (y) {
        var v = Data_Ord.compare(Data_Date.ordDate)(x.value0)(y.value0);
        if (v instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        return Data_Ord.compare(Data_Time.ordTime)(x.value1)(y.value1);
    };
});
var diff = function (dictDuration) {
    return function (dt1) {
        return function (dt2) {
            return Data_Time_Duration.toDuration(dictDuration)($foreign.calcDiff(toRecord(dt1), toRecord(dt2)));
        };
    };
};
var date = function (v) {
    return v.value0;
};
var boundedDateTime = new Data_Bounded.Bounded(function () {
    return ordDateTime;
}, new DateTime(Data_Bounded.bottom(Data_Date.boundedDate), Data_Bounded.bottom(Data_Time.boundedTime)), new DateTime(Data_Bounded.top(Data_Date.boundedDate), Data_Bounded.top(Data_Time.boundedTime)));
var adjust = function (dictDuration) {
    return function (d) {
        return function (dt) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)($foreign.adjustImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value)(Data_Time_Duration.fromDuration(dictDuration)(d))(toRecord(dt)))(function (rec) {
                return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(DateTime.create)(Control_Bind.join(Data_Maybe.bindMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Date.exactDate)(Data_Enum.toEnum(Data_Date_Component.boundedEnumYear)(rec.year)))(Data_Enum.toEnum(Data_Date_Component.boundedEnumMonth)(rec.month)))(Data_Enum.toEnum(Data_Date_Component.boundedEnumDay)(rec.day)))))(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Time.Time.create)(Data_Enum.toEnum(Data_Time_Component.boundedEnumHour)(rec.hour)))(Data_Enum.toEnum(Data_Time_Component.boundedEnumMinute)(rec.minute)))(Data_Enum.toEnum(Data_Time_Component.boundedEnumSecond)(rec.second)))(Data_Enum.toEnum(Data_Time_Component.boundedEnumMillisecond)(rec.millisecond)));
            });
        };
    };
};
module.exports = {
    DateTime: DateTime,
    date: date,
    modifyDate: modifyDate,
    modifyDateF: modifyDateF,
    time: time,
    modifyTime: modifyTime,
    modifyTimeF: modifyTimeF,
    adjust: adjust,
    diff: diff,
    eqDateTime: eqDateTime,
    ordDateTime: ordDateTime,
    boundedDateTime: boundedDateTime,
    showDateTime: showDateTime
};
