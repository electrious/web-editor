"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var Angle = /** @class */ (function () {
    function Angle(deg) {
        this.deg = deg;
    }
    Angle.fromRad = function (rad) {
        return new Angle((rad * 180) / Math.PI);
    };
    Object.defineProperty(Angle.prototype, "rad", {
        get: function () {
            return (this.deg * Math.PI) / 180;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Angle.prototype, "sin", {
        get: function () {
            return Math.sin(this.rad);
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Angle.prototype, "cos", {
        get: function () {
            return Math.cos(this.rad);
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Angle.prototype, "tan", {
        get: function () {
            return Math.tan(this.rad);
        },
        enumerable: true,
        configurable: true
    });
    Angle.prototype.add = function (angle) {
        return new Angle(this.deg + angle.deg);
    };
    Angle.prototype.sub = function (angle) {
        return new Angle(this.deg - angle.deg);
    };
    return Angle;
}());
exports.Angle = Angle;
//# sourceMappingURL=angle.js.map