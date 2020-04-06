const isSimple = require('shamos-hoey')

exports.isSimplePolygon = function(psArr) {
    return isSimple({ type: 'Polygon', coordinates: [psArr] })
}
