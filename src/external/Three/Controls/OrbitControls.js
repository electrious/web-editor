let o = require('three/examples/js/controls/OrbitControls')

exports.mkOrbitControls = camera => domElem => _ => {
    return new o.OrbitControls(camera, domElem)
}