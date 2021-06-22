const three = require('three')
const lines = require('three/examples/jsm/lines/LineGeometry')

exports.mkBufferGeometry = _ => {
    return new three.BufferGeometry();
}

exports.setAttribute = _ => name => attr => geo => _ => {
    geo.setAttribute(name, attr);
}

exports.getAttribute = _ => name => geo => {
    return geo.getAttribute(name);
}

exports.setIndex = _ => idx => geo => _ => {
    geo.setIndex(idx);
}

exports.clone = _ => g => _ => {
    return g.clone()
}

exports.computeVertexNormals = _ => geo => _ => {
    geo.computeVertexNormals();
};

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

exports.mkExtrudeGeometry = s => opt => _ => {
    return new three.ExtrudeGeometry(s, opt)
}

exports.mkLineGeometry = ps => _ => {
    let geo = new lines.LineGeometry()

    if (ps.length > 0) {
        let positions = []
        ps.forEach(p => positions.push(p.x, p.y, p.z))
        geo.setPositions(positions)
    }

    return geo
}


exports.mkBufferAttribute = _ => arr => s => _ => {
    return new three.BufferAttribute(arr, s);
}

exports.isBufferAttribute = attr => {
    return attr instanceof three.BufferAttribute
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
