exports.makeTappable = _ => obj => cb => _ => {
    obj.tapped = cb
}

exports.stopTappable = _ => obj => _ => {
    obj.tapped = undefined
}

exports.isTappable = _ => obj => {
    return obj.tapped !== undefined
}

exports.sendTapEvent = _ => obj => evt => _ => {
    obj.tapped(evt)()
}

exports.makeMouseMove = _ => obj => cb => _ => {
    obj.mouseMove = cb
}

exports.stopMouseMove = _ => obj => _ => {
    obj.mouseMove = undefined
}

exports.isMouseMove = _ => obj => {
    return obj.mouseMove !== undefined
}

exports.sendMouseMoveEvent = _ => obj => evt => _ => {
    obj.mouseMove(evt)()
}

exports.makeDraggable = _ => obj => cb => _ => {
    obj.dragged = cb
}

exports.stopDraggable = _ => obj => _ => {
    obj.dragged = undefined
}

exports.isDraggable = _ => obj => {
    return obj.dragged !== undefined
}

exports.sendDragEvent = _ => obj => evt => _ => {
    obj.dragged(evt)()
}