const clipper = require('clipper-lib')

exports.mkClipper = _ => {
    return new clipper.Clipper()
}

exports.mkIntPoint = x => y => {
    return new clipper.IntPoint(x, y)
}

exports.ptSubject = clipper.PolyType.ptSubject
exports.ptClip = clipper.PolyType.ptClip

exports.ctIntersection = clipper.ClipType.ctIntersection
exports.ctUnion = clipper.ClipType.ctUnion
exports.ctDifference = clipper.ClipType.ctDifference
exports.ctXor = clipper.ClipType.ctXor

exports.pftEvenOdd = clipper.PolyFillType.pftEvenOdd
exports.pftNonZero = clipper.PolyFillType.pftNonZero
exports.pftPositive = clipper.PolyFillType.pftPositive
exports.pftNegative = clipper.PolyFillType.pftNegative

exports.addPath = path => polyType => closed => clipper => _ => {
    clipper.AddPath(path, polyType, closed)
}

exports.addPaths = paths => polyType => closed => clipper => _ => {
    clipper.AddPaths(paths, polyType, closed)
}

exports.clear = clipper => _ => {
    clipper.Clear()
}

exports.area = poly => {
    return clipper.Clipper.Area(poly)
}

exports.cleanPolygon = poly => distance => _ => {
    return clipper.Clipper.CleanPolygon(poly, distance)
}

exports.cleanPolygons = polys => distance => _ => {
    return clipper.Clipper.CleanPolygons(polys, distance)
}

exports.execute = clipType => c => _ => {
    var solution = new clipper.Paths()
    c.Execute(clipType, solution)
    return solution
}

exports.polyOrientation = path => {
    return clipper.Clipper.Orientation(path)
}

exports.pointInPolygon = pt => path => {
    return clipper.Clipper.PointInPolygon(pt, path) != 0
}

exports.reversePath = p => {
    return p.reverse()
}

exports.reversePaths = ps => {
    clipper.Clipper.ReversePaths(ps)
    return ps
}

exports.simplifyPolygon = path => fillType => _ => {
    return clipper.Clipper.SimplifyPolygon(path, fillType)
}

exports.simplifyPolygons = paths => fillType => _ => {
    return clipper.Clipper.SimplifyPolygons(paths, fillType)
}