exports.getBoundingClientRect = el => {
    const rect = el.getBoundingClientRect()
    return {
        top    : rect.top,
        right  : rect.right,
        bottom : rect.bottom,
        left   : rect.left,
        width  : rect.width,
        height : rect.height
    }
}
