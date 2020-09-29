const three = require('three')

exports.mkPerspectiveCamera = fov => aspect => near => far => _ => {
    return new three.PerspectiveCamera(fov, aspect, near, far)
}