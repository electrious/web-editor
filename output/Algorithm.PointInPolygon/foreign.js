/**
 * test if a 2d point is inside a polygon
 * @param polygon
 * @param point
 */
exports.pointInPolygon = (polygon, point) => {
    // ray-casting algorithm based on
    // http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html

    const x = point.x
    const y = point.y

    let inside = false
    const len = polygon.length

    for (let i = 0, j = len - 1; i < len; j = i++) {
        const xi = polygon[i].x,
            yi = polygon[i].y
        const xj = polygon[j].x,
            yj = polygon[j].y

        const intersect =
            yi > y != yj > y && x < ((xj - xi) * (y - yi)) / (yj - yi) + xi
        if (intersect) inside = !inside
    }

    return inside
}
