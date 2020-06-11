let o = require('three/examples/jsm/controls/OrbitControls')

exports.mkOrbitControls = _ => camera => domElem => _ => {
    return new o.OrbitControls(camera, domElem)
}