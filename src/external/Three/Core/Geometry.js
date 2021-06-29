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


exports.vertices = _ => geo => {
    let attr = geo.getAttribute("position");
    let arr = attr.array;

    let al = arr.length / 3;
    
    let vs = new Array(al);

    for (var i = 0; i < al; i++) {
        let j = i * 3;
        vs[i] = new three.Vector3(arr[j], arr[j + 1], arr[j + 2]);
    }

    return vs;
};

exports.faces = _ => geo => {
    var attr = geo.getIndex();
    let arr = attr.array;
    let al = arr.length / 3;
    let fs = new Array(al);

    for (var i = 0; i < al; i++) {
        let j = i * 3;
        fs[i] = {
            a: arr[j],
            b: arr[j + 1],
            c: arr[j + 2]
        };
    }

    return fs;
};

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

exports.mkCylinderGeometry = topRadius => botRadius => height => radialSegs => _ => {
    return new three.CylinderGeometry(topRadius, botRadius, height, radialSegs);
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
