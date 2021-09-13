const three = require('three');

exports.mkPath = _ => {
    return new three.Path();
}

exports.jsMoveTo = x => y => p => _ => {
    p.moveTo(x, y);
}

exports.jsLineTo = x => y => p => _ => {
    p.lineTo(x, y);
}

exports.jsBezierCurveTo = cp1x => cp1y => cp2x => cp2y => x => y => _ => {
    p.bezierCurveTo(cp1x, cp1y, cp2x, cp2y)
}
