const three = require('three')

exports.mkBoxGeometry = width => height => depth => _ => {
    return new three.BoxGeometry(width, height, depth)
}

exports.mkCircleGeometry = radius => segs => _ => {
    return new three.CircleGeometry(radius, segs)
}

exports.mkShape = ps => _ => {
    return new three.Shape(ps)
}

exports.mkShapeGeometry = shp => _ => {
    return new three.ShapeGeometry(shp)
}

exports.isBufferGeometry = geo => {
    return geo instanceof three.BufferGeometry
}

exports.isBufferAttribute = attr => {
    return attr instanceof three.BufferAttribute
}