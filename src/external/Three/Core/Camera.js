const three = require('three')

exports.mkPerspectiveCamera = fov => {
    return aspect => {
        return near => {
            return far => {
                return _ => {
                    return new three.PerspectiveCamera(fov, aspect, near, far)
                }
            }
        }
    }
}