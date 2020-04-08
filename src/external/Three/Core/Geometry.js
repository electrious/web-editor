const three = require('three')

exports.mkCircleGeometry = radius => {
    return segs => {
        return _ => {
            return new three.CircleGeometry(radius, segs)
        }
    }
}

exports.mkShape = ps => {
    return _ => {
        return new three.Shape(ps)
    }
}

exports.mkShapeGeometry = shp => {
    return _ => {
        return new three.ShapeGeometry(shp)
    }
}

exports.isBufferGeometry = geo => {
    return geo instanceof three.BufferGeometry
}

exports.isBufferAttribute = attr => {
    return attr instanceof three.BufferAttribute
}