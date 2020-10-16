const three = require('three')

exports.mkGeometry = _ => {
    return new three.Geometry()
}

exports.mkBoxGeometry = width => height => depth => _ => {
    return new three.BoxGeometry(width, height, depth)
}

exports.mkCircleGeometry = radius => segs => _ => {
    return new three.CircleGeometry(radius, segs)
}

exports.mkCylinderGeometry = radius => height => _ => {
    return new three.CylinderGeometry(radius, radius, height)
}

exports.mkShape = ps => _ => {
    return new three.Shape(ps)
}

exports.mkShapeGeometry = shp => _ => {
    return new three.ShapeGeometry(shp)
}

exports.isBufferAttribute = attr => {
    return attr instanceof three.BufferAttribute
}