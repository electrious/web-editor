"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/**
 * test if a 2d point is inside a polygon
 * @param polygon
 * @param point
 */
function pointInPolygon(polygon, point) {
    // ray-casting algorithm based on
    // http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
    var x = point.x;
    var y = point.y;
    var inside = false;
    var len = polygon.length;
    for (var i = 0, j = len - 1; i < len; j = i++) {
        var xi = polygon[i].x, yi = polygon[i].y;
        var xj = polygon[j].x, yj = polygon[j].y;
        var intersect = yi > y != yj > y && x < ((xj - xi) * (y - yi)) / (yj - yi) + xi;
        if (intersect)
            inside = !inside;
    }
    return inside;
}
exports.pointInPolygon = pointInPolygon;
//# sourceMappingURL=pointinpolygon.js.map