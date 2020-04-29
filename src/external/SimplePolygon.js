const shamos = require('shamos-hoey')
const isSimple = shamos.default

exports.isSimplePolygon = function (psArr) {
    return isSimple({
        type: 'Polygon',
        coordinates: [psArr]
    })
}
