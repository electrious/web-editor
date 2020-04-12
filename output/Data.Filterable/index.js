// Generated by purs version 0.13.6
"use strict";
var Control_Bind = require("../Control.Bind/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Compactable = require("../Data.Compactable/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_List = require("../Data.List/index.js");
var Data_List_Types = require("../Data.List.Types/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Filterable = function (Compactable0, Functor1, filter, filterMap, partition, partitionMap) {
    this.Compactable0 = Compactable0;
    this.Functor1 = Functor1;
    this.filter = filter;
    this.filterMap = filterMap;
    this.partition = partition;
    this.partitionMap = partitionMap;
};
var partitionMapDefault = function (dictFilterable) {
    return function (p) {
        var $82 = Data_Compactable.separate(dictFilterable.Compactable0());
        var $83 = Data_Functor.map(dictFilterable.Functor1())(p);
        return function ($84) {
            return $82($83($84));
        };
    };
};
var partitionMap = function (dict) {
    return dict.partitionMap;
};
var partition = function (dict) {
    return dict.partition;
};
var maybeBool = function (p) {
    return function (x) {
        var $33 = p(x);
        if ($33) {
            return new Data_Maybe.Just(x);
        };
        return Data_Maybe.Nothing.value;
    };
};
var filterableList = new Filterable(function () {
    return Data_Compactable.compactableList;
}, function () {
    return Data_List_Types.functorList;
}, Data_List.filter, function (p) {
    return Data_List.mapMaybe(p);
}, function (p) {
    return function (xs) {
        var select = function (x) {
            return function (v) {
                var $36 = p(x);
                if ($36) {
                    return {
                        no: v.no,
                        yes: new Data_List_Types.Cons(x, v.yes)
                    };
                };
                return {
                    no: new Data_List_Types.Cons(x, v.no),
                    yes: v.yes
                };
            };
        };
        return Data_Foldable.foldr(Data_List_Types.foldableList)(select)({
            no: Data_List_Types.Nil.value,
            yes: Data_List_Types.Nil.value
        })(xs);
    };
}, function (p) {
    return function (xs) {
        var select = function (x) {
            return function (v) {
                var v1 = p(x);
                if (v1 instanceof Data_Either.Left) {
                    return {
                        left: new Data_List_Types.Cons(v1.value0, v.left),
                        right: v.right
                    };
                };
                if (v1 instanceof Data_Either.Right) {
                    return {
                        left: v.left,
                        right: new Data_List_Types.Cons(v1.value0, v.right)
                    };
                };
                throw new Error("Failed pattern match at Data.Filterable (line 190, column 36 - line 192, column 83): " + [ v1.constructor.name ]);
            };
        };
        return Data_Foldable.foldr(Data_List_Types.foldableList)(select)({
            left: Data_List_Types.Nil.value,
            right: Data_List_Types.Nil.value
        })(xs);
    };
});
var filterableArray = new Filterable(function () {
    return Data_Compactable.compactableArray;
}, function () {
    return Data_Functor.functorArray;
}, Data_Array.filter, Data_Array.mapMaybe, Data_Array.partition, function (p) {
    var go = function (acc) {
        return function (x) {
            var v = p(x);
            if (v instanceof Data_Either.Left) {
                return {
                    left: Data_Semigroup.append(Data_Semigroup.semigroupArray)(acc.left)([ v.value0 ]),
                    right: acc.right
                };
            };
            if (v instanceof Data_Either.Right) {
                return {
                    right: Data_Semigroup.append(Data_Semigroup.semigroupArray)(acc.right)([ v.value0 ]),
                    left: acc.left
                };
            };
            throw new Error("Failed pattern match at Data.Filterable (line 149, column 16 - line 151, column 50): " + [ v.constructor.name ]);
        };
    };
    return Data_Foldable.foldl(Data_Foldable.foldableArray)(go)({
        left: [  ],
        right: [  ]
    });
});
var filterMapDefault = function (dictFilterable) {
    return function (p) {
        var $85 = Data_Compactable.compact(dictFilterable.Compactable0());
        var $86 = Data_Functor.map(dictFilterable.Functor1())(p);
        return function ($87) {
            return $85($86($87));
        };
    };
};
var filterMap = function (dict) {
    return dict.filterMap;
};
var partitionDefaultFilterMap = function (dictFilterable) {
    return function (p) {
        return function (xs) {
            return {
                yes: filterMap(dictFilterable)(maybeBool(p))(xs),
                no: filterMap(dictFilterable)(maybeBool(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraBoolean))(p)))(xs)
            };
        };
    };
};
var filterDefaultPartition = function (dictFilterable) {
    return function (p) {
        return function (xs) {
            return (partition(dictFilterable)(p)(xs)).yes;
        };
    };
};
var filterDefault = function (dictFilterable) {
    var $88 = filterMap(dictFilterable);
    return function ($89) {
        return $88(maybeBool($89));
    };
};
var filter = function (dict) {
    return dict.filter;
};
var partitionDefaultFilter = function (dictFilterable) {
    return function (p) {
        return function (xs) {
            return {
                yes: filter(dictFilterable)(p)(xs),
                no: filter(dictFilterable)(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraBoolean))(p))(xs)
            };
        };
    };
};
var eitherBool = function (p) {
    return function (x) {
        var $49 = p(x);
        if ($49) {
            return new Data_Either.Right(x);
        };
        return new Data_Either.Left(x);
    };
};
var filterDefaultPartitionMap = function (dictFilterable) {
    return function (p) {
        return function (xs) {
            return (partitionMap(dictFilterable)(eitherBool(p))(xs)).right;
        };
    };
};
var partitionDefault = function (dictFilterable) {
    return function (p) {
        return function (xs) {
            var o = partitionMap(dictFilterable)(eitherBool(p))(xs);
            return {
                no: o.left,
                yes: o.right
            };
        };
    };
};
var filterableEither = function (dictMonoid) {
    return new Filterable(function () {
        return Data_Compactable.compactableEither(dictMonoid);
    }, function () {
        return Data_Either.functorEither;
    }, function (p) {
        return filterDefault(filterableEither(dictMonoid))(p);
    }, function (p) {
        return function (v) {
            if (v instanceof Data_Either.Left) {
                return new Data_Either.Left(v.value0);
            };
            if (v instanceof Data_Either.Right) {
                var v1 = p(v.value0);
                if (v1 instanceof Data_Maybe.Nothing) {
                    return new Data_Either.Left(Data_Monoid.mempty(dictMonoid));
                };
                if (v1 instanceof Data_Maybe.Just) {
                    return new Data_Either.Right(v1.value0);
                };
                throw new Error("Failed pattern match at Data.Filterable (line 180, column 27 - line 182, column 22): " + [ v1.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.Filterable (line 171, column 1 - line 184, column 29): " + [ p.constructor.name, v.constructor.name ]);
        };
    }, function (p) {
        return partitionDefault(filterableEither(dictMonoid))(p);
    }, function (p) {
        return function (v) {
            if (v instanceof Data_Either.Left) {
                return {
                    left: new Data_Either.Left(v.value0),
                    right: new Data_Either.Left(v.value0)
                };
            };
            if (v instanceof Data_Either.Right) {
                var v1 = p(v.value0);
                if (v1 instanceof Data_Either.Left) {
                    return {
                        left: new Data_Either.Right(v1.value0),
                        right: new Data_Either.Left(Data_Monoid.mempty(dictMonoid))
                    };
                };
                if (v1 instanceof Data_Either.Right) {
                    return {
                        left: new Data_Either.Left(Data_Monoid.mempty(dictMonoid)),
                        right: new Data_Either.Right(v1.value0)
                    };
                };
                throw new Error("Failed pattern match at Data.Filterable (line 173, column 30 - line 175, column 53): " + [ v1.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.Filterable (line 171, column 1 - line 184, column 29): " + [ p.constructor.name, v.constructor.name ]);
        };
    });
};
var filterableMap = function (dictOrd) {
    return new Filterable(function () {
        return Data_Compactable.compactableMap(dictOrd);
    }, function () {
        return Data_Map_Internal.functorMap;
    }, function (p) {
        return filterDefault(filterableMap(dictOrd))(p);
    }, function (p) {
        return function (xs) {
            var toList = Data_Map_Internal.toUnfoldable(Data_List_Types.unfoldableList);
            var select = function (v) {
                return function (m) {
                    return Data_Map_Internal.alter(dictOrd)(Data_Function["const"](p(v.value1)))(v.value0)(m);
                };
            };
            return Data_Foldable.foldr(Data_List_Types.foldableList)(select)(Data_Map_Internal.empty)(toList(xs));
        };
    }, function (p) {
        return partitionDefault(filterableMap(dictOrd))(p);
    }, function (p) {
        return function (xs) {
            var toList = Data_Map_Internal.toUnfoldable(Data_List_Types.unfoldableList);
            var select = function (v) {
                return function (v1) {
                    var v2 = p(v.value1);
                    if (v2 instanceof Data_Either.Left) {
                        return {
                            left: Data_Map_Internal.insert(dictOrd)(v.value0)(v2.value0)(v1.left),
                            right: v1.right
                        };
                    };
                    if (v2 instanceof Data_Either.Right) {
                        return {
                            left: v1.left,
                            right: Data_Map_Internal.insert(dictOrd)(v.value0)(v2.value0)(v1.right)
                        };
                    };
                    throw new Error("Failed pattern match at Data.Filterable (line 215, column 44 - line 217, column 57): " + [ v2.constructor.name ]);
                };
            };
            return Data_Foldable.foldr(Data_List_Types.foldableList)(select)({
                left: Data_Map_Internal.empty,
                right: Data_Map_Internal.empty
            })(toList(xs));
        };
    });
};
var filterableMaybe = new Filterable(function () {
    return Data_Compactable.compactableMaybe;
}, function () {
    return Data_Maybe.functorMaybe;
}, function (p) {
    return filterDefault(filterableMaybe)(p);
}, Control_Bind.bindFlipped(Data_Maybe.bindMaybe), function (p) {
    return partitionDefault(filterableMaybe)(p);
}, function (p) {
    return function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return {
                left: Data_Maybe.Nothing.value,
                right: Data_Maybe.Nothing.value
            };
        };
        if (v instanceof Data_Maybe.Just) {
            var v1 = p(v.value0);
            if (v1 instanceof Data_Either.Left) {
                return {
                    left: new Data_Maybe.Just(v1.value0),
                    right: Data_Maybe.Nothing.value
                };
            };
            if (v1 instanceof Data_Either.Right) {
                return {
                    left: Data_Maybe.Nothing.value,
                    right: new Data_Maybe.Just(v1.value0)
                };
            };
            throw new Error("Failed pattern match at Data.Filterable (line 161, column 29 - line 163, column 48): " + [ v1.constructor.name ]);
        };
        throw new Error("Failed pattern match at Data.Filterable (line 159, column 1 - line 169, column 29): " + [ p.constructor.name, v.constructor.name ]);
    };
});
var cleared = function (dictFilterable) {
    return filterMap(dictFilterable)(Data_Function["const"](Data_Maybe.Nothing.value));
};
module.exports = {
    Filterable: Filterable,
    partitionMap: partitionMap,
    partition: partition,
    filterMap: filterMap,
    filter: filter,
    eitherBool: eitherBool,
    partitionDefault: partitionDefault,
    partitionDefaultFilter: partitionDefaultFilter,
    partitionDefaultFilterMap: partitionDefaultFilterMap,
    partitionMapDefault: partitionMapDefault,
    maybeBool: maybeBool,
    filterDefault: filterDefault,
    filterDefaultPartition: filterDefaultPartition,
    filterDefaultPartitionMap: filterDefaultPartitionMap,
    filterMapDefault: filterMapDefault,
    cleared: cleared,
    filterableArray: filterableArray,
    filterableMaybe: filterableMaybe,
    filterableEither: filterableEither,
    filterableList: filterableList,
    filterableMap: filterableMap
};