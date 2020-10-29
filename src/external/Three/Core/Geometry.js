const three = require('three')

exports.clone = _ => g => _ => {
    return g.clone()
}

exports.mkGeometry = _ => {
    return new three.Geometry()
}

exports.vertices = _ => geo => {
    return geo.vertices
}

exports.setVertices = _ => vs => geo => _ => {
    geo.vertices = vs
}

exports.setVerticesNeedUpdate = _ => u => geo => _ => {
    geo.verticesNeedUpdate = u
}

exports.faces = _ => geo => {
    return geo.faces
}

exports.setFaces = _ => fs => geo => _ => {
    geo.faces = fs
}

exports.setElementsNeedUpdate = _ => u => geo => _ => {
    geo.elementsNeedUpdate = u
}

exports.setUVs = _ => uvs => geo => _ => {
    geo.faceVertexUvs = [uvs]
}

exports.setUVsNeedUpdate = _ => u => geo => _ => {
    geo.uvsNeedUpdate = u
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

exports.mkPlaneGeometry = w => h => wSegs => hSegs => _ => {
    return new three.PlaneGeometry(w, h, wSegs, hSegs)
}

exports.isBufferAttribute = attr => {
    return attr instanceof three.BufferAttribute
}

exports.getAttribute = _ => name => geo => {
    return geo.getAttribute(name)
}

exports.setXYZ = idx => x => y => z => attr => _ => {
    attr.setXYZ(idx, x, y, z)
}

exports.setNeedsUpdate = u => attr => _ => {
    attr.needsUpdate = u
}

exports.count = attr => {
    return attr.count
}

exports.getX = idx => attr => {
    return attr.getX(idx)
}

exports.getY = idx => attr => {
    return attr.getY(idx)
}

exports.getZ = idx => attr => {
    return attr.getZ(idx)
}
