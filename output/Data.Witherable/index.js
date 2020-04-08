// Generated by purs version 0.13.6
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Category = require("../Control.Category/index.js");
var Data_Compactable = require("../Data.Compactable/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Filterable = require("../Data.Filterable/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_List = require("../Data.List/index.js");
var Data_List_Types = require("../Data.List.Types/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Witherable = function (Filterable0, Traversable1, wilt, wither) {
    this.Filterable0 = Filterable0;
    this.Traversable1 = Traversable1;
    this.wilt = wilt;
    this.wither = wither;
};
var witherableMaybe = new Witherable(function () {
    return Data_Filterable.filterableMaybe;
}, function () {
    return Data_Traversable.traversableMaybe;
}, function (dictApplicative) {
    return function (p) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return Control_Applicative.pure(dictApplicative)({
                    left: Data_Maybe.Nothing.value,
                    right: Data_Maybe.Nothing.value
                });
            };
            if (v instanceof Data_Maybe.Just) {
                var convert = function (v1) {
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
                    throw new Error("Failed pattern match at Data.Witherable (line 147, column 5 - line 147, column 56): " + [ v1.constructor.name ]);
                };
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(convert)(p(v.value0));
            };
            throw new Error("Failed pattern match at Data.Witherable (line 144, column 1 - line 151, column 26): " + [ p.constructor.name, v.constructor.name ]);
        };
    };
}, function (dictApplicative) {
    return function (p) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value);
            };
            if (v instanceof Data_Maybe.Just) {
                return p(v.value0);
            };
            throw new Error("Failed pattern match at Data.Witherable (line 144, column 1 - line 151, column 26): " + [ p.constructor.name, v.constructor.name ]);
        };
    };
});
var witherableMap = function (dictOrd) {
    return new Witherable(function () {
        return Data_Filterable.filterableMap(dictOrd);
    }, function () {
        return Data_Map_Internal.traversableMap;
    }, function (dictApplicative) {
        return function (p) {
            var toList = function (dictOrd1) {
                return Data_Map_Internal.toUnfoldable(Data_List_Types.unfoldableList);
            };
            var go = function (acc) {
                return function (v) {
                    return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(function (v1) {
                        return function (v2) {
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
                            throw new Error("Failed pattern match at Data.Witherable (line 128, column 9 - line 130, column 59): " + [ v2.constructor.name ]);
                        };
                    })(acc))(p(v.value1));
                };
            };
            var $91 = Data_Foldable.foldl(Data_List_Types.foldableList)(go)(Control_Applicative.pure(dictApplicative)({
                left: Data_Map_Internal.empty,
                right: Data_Map_Internal.empty
            }));
            var $92 = toList(dictOrd);
            return function ($93) {
                return $91($92($93));
            };
        };
    }, function (dictApplicative) {
        return function (p) {
            var toList = function (dictOrd1) {
                return Data_Map_Internal.toUnfoldable(Data_List_Types.unfoldableList);
            };
            var go = function (acc) {
                return function (v) {
                    return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(function (comp) {
                        return function (v1) {
                            if (v1 instanceof Data_Maybe.Nothing) {
                                return comp;
                            };
                            if (v1 instanceof Data_Maybe.Just) {
                                return Data_Map_Internal.insert(dictOrd)(v.value0)(v1.value0)(comp);
                            };
                            throw new Error("Failed pattern match at Data.Witherable (line 139, column 9 - line 141, column 41): " + [ v1.constructor.name ]);
                        };
                    })(acc))(p(v.value1));
                };
            };
            var $94 = Data_Foldable.foldl(Data_List_Types.foldableList)(go)(Control_Applicative.pure(dictApplicative)(Data_Map_Internal.empty));
            var $95 = toList(dictOrd);
            return function ($96) {
                return $94($95($96));
            };
        };
    });
};
var witherableList = new Witherable(function () {
    return Data_Filterable.filterableList;
}, function () {
    return Data_List_Types.traversableList;
}, function (dictApplicative) {
    return function (p) {
        var rev = function (v) {
            return {
                left: Data_List.reverse(v.left),
                right: Data_List.reverse(v.right)
            };
        };
        var go = function (acc) {
            return function (x) {
                return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(function (v) {
                    return function (v1) {
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
                        throw new Error("Failed pattern match at Data.Witherable (line 109, column 7 - line 111, column 46): " + [ v1.constructor.name ]);
                    };
                })(acc))(p(x));
            };
        };
        var $97 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(rev);
        var $98 = Data_Foldable.foldl(Data_List_Types.foldableList)(go)(Control_Applicative.pure(dictApplicative)({
            left: Data_List_Types.Nil.value,
            right: Data_List_Types.Nil.value
        }));
        return function ($99) {
            return $97($98($99));
        };
    };
}, function (dictApplicative) {
    return function (p) {
        var go = function (acc) {
            return function (x) {
                return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(function (comp) {
                    return function (v) {
                        if (v instanceof Data_Maybe.Nothing) {
                            return comp;
                        };
                        if (v instanceof Data_Maybe.Just) {
                            return new Data_List_Types.Cons(v.value0, comp);
                        };
                        throw new Error("Failed pattern match at Data.Witherable (line 116, column 7 - line 118, column 28): " + [ v.constructor.name ]);
                    };
                })(acc))(p(x));
            };
        };
        var $100 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_List.reverse);
        var $101 = Data_Foldable.foldl(Data_List_Types.foldableList)(go)(Control_Applicative.pure(dictApplicative)(Data_List_Types.Nil.value));
        return function ($102) {
            return $100($101($102));
        };
    };
});
var witherableEither = function (dictMonoid) {
    return new Witherable(function () {
        return Data_Filterable.filterableEither(dictMonoid);
    }, function () {
        return Data_Either.traversableEither;
    }, function (dictApplicative) {
        return function (p) {
            return function (v) {
                if (v instanceof Data_Either.Left) {
                    return Control_Applicative.pure(dictApplicative)({
                        left: new Data_Either.Left(v.value0),
                        right: new Data_Either.Left(v.value0)
                    });
                };
                if (v instanceof Data_Either.Right) {
                    var convert = function (v1) {
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
                        throw new Error("Failed pattern match at Data.Witherable (line 156, column 5 - line 156, column 61): " + [ v1.constructor.name ]);
                    };
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(convert)(p(v.value0));
                };
                throw new Error("Failed pattern match at Data.Witherable (line 153, column 1 - line 162, column 31): " + [ p.constructor.name, v.constructor.name ]);
            };
        };
    }, function (dictApplicative) {
        return function (p) {
            return function (v) {
                if (v instanceof Data_Either.Left) {
                    return Control_Applicative.pure(dictApplicative)(new Data_Either.Left(v.value0));
                };
                if (v instanceof Data_Either.Right) {
                    var convert = function (v1) {
                        if (v1 instanceof Data_Maybe.Nothing) {
                            return new Data_Either.Left(Data_Monoid.mempty(dictMonoid));
                        };
                        if (v1 instanceof Data_Maybe.Just) {
                            return new Data_Either.Right(v1.value0);
                        };
                        throw new Error("Failed pattern match at Data.Witherable (line 161, column 5 - line 161, column 34): " + [ v1.constructor.name ]);
                    };
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(convert)(p(v.value0));
                };
                throw new Error("Failed pattern match at Data.Witherable (line 153, column 1 - line 162, column 31): " + [ p.constructor.name, v.constructor.name ]);
            };
        };
    });
};
var witherDefault = function (dictWitherable) {
    return function (dictApplicative) {
        return function (p) {
            var $103 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Compactable.compact((dictWitherable.Filterable0()).Compactable0()));
            var $104 = Data_Traversable.traverse(dictWitherable.Traversable1())(dictApplicative)(p);
            return function ($105) {
                return $103($104($105));
            };
        };
    };
};
var wither = function (dict) {
    return dict.wither;
};
var withered = function (dictWitherable) {
    return function (dictApplicative) {
        return wither(dictWitherable)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
    };
};
var wiltDefault = function (dictWitherable) {
    return function (dictApplicative) {
        return function (p) {
            var $106 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Compactable.separate((dictWitherable.Filterable0()).Compactable0()));
            var $107 = Data_Traversable.traverse(dictWitherable.Traversable1())(dictApplicative)(p);
            return function ($108) {
                return $106($107($108));
            };
        };
    };
};
var witherableArray = new Witherable(function () {
    return Data_Filterable.filterableArray;
}, function () {
    return Data_Traversable.traversableArray;
}, function (dictApplicative) {
    return wiltDefault(witherableArray)(dictApplicative);
}, function (dictApplicative) {
    return witherDefault(witherableArray)(dictApplicative);
});
var wilt = function (dict) {
    return dict.wilt;
};
var wilted = function (dictWitherable) {
    return function (dictApplicative) {
        return wilt(dictWitherable)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
    };
};
var traverseByWither = function (dictWitherable) {
    return function (dictApplicative) {
        return function (f) {
            return wither(dictWitherable)(dictApplicative)((function () {
                var $109 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe.Just.create);
                return function ($110) {
                    return $109(f($110));
                };
            })());
        };
    };
};
var partitionMapByWilt = function (dictWitherable) {
    return function (p) {
        var $111 = Data_Newtype.unwrap(Data_Identity.newtypeIdentity);
        var $112 = wilt(dictWitherable)(Data_Identity.applicativeIdentity)(function ($114) {
            return Data_Identity.Identity(p($114));
        });
        return function ($113) {
            return $111($112($113));
        };
    };
};
var filterMapByWither = function (dictWitherable) {
    return function (p) {
        var $115 = Data_Newtype.unwrap(Data_Identity.newtypeIdentity);
        var $116 = wither(dictWitherable)(Data_Identity.applicativeIdentity)(function ($118) {
            return Data_Identity.Identity(p($118));
        });
        return function ($117) {
            return $115($116($117));
        };
    };
};
module.exports = {
    Witherable: Witherable,
    wilt: wilt,
    wither: wither,
    partitionMapByWilt: partitionMapByWilt,
    filterMapByWither: filterMapByWither,
    traverseByWither: traverseByWither,
    wilted: wilted,
    withered: withered,
    witherDefault: witherDefault,
    wiltDefault: wiltDefault,
    witherableArray: witherableArray,
    witherableList: witherableList,
    witherableMap: witherableMap,
    witherableMaybe: witherableMaybe,
    witherableEither: witherableEither
};
