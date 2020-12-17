exports.makeTappableJS = obj => cb => _ => {
    obj.tapped = cb
}

exports.stopTappableJS = obj => _ => {
    obj.tapped = undefined
}

exports.isTappableJS = obj => {
    return obj.tapped !== undefined
}

exports.sendTapEventJS = obj => evt => _ => {
    obj.tapped(evt)()
}

exports.makeMouseMoveJS = obj => cb => _ => {
    obj.mouseMove = cb
}

exports.stopMouseMoveJS = obj => _ => {
    obj.mouseMove = undefined
}

exports.isMouseMoveJS = obj => {
    return obj.mouseMove !== undefined
}

exports.sendMouseMoveEventJS = obj => evt => _ => {
    obj.mouseMove(evt)()
}

exports.makeDraggableJS = obj => cb => _ => {
    obj.dragged = cb
}

exports.stopDraggableJS = obj => _ => {
    obj.dragged = undefined
}

exports.isDraggableJS = obj => {
    return obj.dragged !== undefined
}

exports.sendDragEventJS = obj => evt => _ => {
    obj.dragged(evt)()
}
