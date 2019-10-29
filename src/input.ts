import { Stream } from '@most/types'
import {
    merge,
    map,
    delay,
    startWith,
    mergeArray,
    constant,
    loop,
    multicast,
    debounce
} from '@most/core'
import {
    domEvent,
    mousedown,
    mousemove,
    mouseup,
    touchstart,
    touchend,
    touchmove
} from '@most/dom-event'
import { curry } from 'ramda'
import { gate, unwrap } from './helper'
import { Vector2 } from 'three'

export interface TapEvent {
    tapX: number
    tapY: number
}

/**
 * Tap gesture recognizer for touches
 * @param elem
 */
function tapped(
    start: Stream<TapEvent>,
    end: Stream<TapEvent>
): Stream<TapEvent> {
    const canBeTap = startWith(
        false,
        merge(constant(false, start), constant(true, end))
    )
    // the touch should end in less than 0.32 seconds to be considered a tap.
    const tapCheckEvt = delay(320, start)
    return gate(canBeTap, tapCheckEvt)
}

export enum DragType {
    DragStart,
    Drag,
    DragEnd
}

export interface DragEvent {
    dragType: DragType
    dragX: number
    dragY: number
    deltaX: number
    deltaY: number
}

// internal enum for managing drag state
enum DragState {
    Waiting,
    MayStart,
    Dragging,
    End
}

// internal drag state
interface DragInfo {
    state: DragState
    lastPos: Vector2
}

// internal function to process drag events with DragInfo as a state between
// events
const processDrag = (
    di: DragInfo,
    de: DragEvent
): { seed: DragInfo; value: DragEvent | null } => {
    const pos = new Vector2(de.dragX, de.dragY)
    if (
        de.dragType == DragType.DragStart &&
        (di.state == DragState.Waiting || di.state == DragState.End)
    ) {
        // drag start
        const info: DragInfo = {
            state: DragState.MayStart,
            lastPos: pos
        }

        return { seed: info, value: null }
    } else if (de.dragType == DragType.Drag) {
        if (di.state == DragState.MayStart && di.lastPos.distanceTo(pos) > 0) {
            // now we received real drag events and we can send out real drag events
            const info: DragInfo = {
                state: DragState.Dragging,
                lastPos: pos
            }

            const evt: DragEvent = {
                dragType: DragType.DragStart,
                dragX: pos.x,
                dragY: pos.y,
                deltaX: pos.x - di.lastPos.x,
                deltaY: pos.y - di.lastPos.y
            }

            return { seed: info, value: evt }
        } else if (di.state == DragState.Dragging) {
            const info: DragInfo = {
                state: di.state,
                lastPos: pos
            }
            const evt: DragEvent = {
                dragType: DragType.Drag,
                dragX: pos.x,
                dragY: pos.y,
                deltaX: pos.x - di.lastPos.x,
                deltaY: pos.y - di.lastPos.y
            }

            return { seed: info, value: evt }
        }
    } else if (
        de.dragType == DragType.DragEnd &&
        di.state == DragState.Dragging
    ) {
        const info: DragInfo = {
            state: DragState.End,
            lastPos: pos
        }
        const evt: DragEvent = {
            dragType: DragType.DragEnd,
            dragX: pos.x,
            dragY: pos.y,
            deltaX: pos.x - di.lastPos.x,
            deltaY: pos.y - di.lastPos.y
        }

        return { seed: info, value: evt }
    }

    return { seed: di, value: null }
}

function mkDragEndable(evt: Stream<DragEvent>): Stream<DragEvent> {
    // wait for 2 seconds and see if there're new events
    // if not, make sure the last one is DragEnd
    const e = debounce(2000, evt)

    const f = (e: DragEvent): DragEvent | null => {
        if (e.dragType != DragType.DragEnd) {
            return {
                dragType: DragType.DragEnd,
                dragX: e.dragX,
                dragY: e.dragY,
                deltaX: 0,
                deltaY: 0
            }
        }

        return null
    }

    return merge(evt, unwrap(map(f, e)))
}

/**
 * Drag gesture recognizer for both mouse and touch events.
 * @param start
 * @param move
 * @param end
 */
function dragged(
    start: Stream<TapEvent>,
    move: Stream<TapEvent>,
    end: Stream<TapEvent>
): Stream<DragEvent> {
    const mkDrag = curry(
        (t: DragType, e: TapEvent): DragEvent => {
            return {
                dragType: t,
                dragX: e.tapX,
                dragY: e.tapY,
                deltaX: 0,
                deltaY: 0
            }
        }
    )
    const e = mergeArray([
        map(mkDrag(DragType.DragStart), start),
        map(mkDrag(DragType.Drag), move),
        map(mkDrag(DragType.DragEnd), end)
    ])

    const defPos = new Vector2(0, 0)
    const def: DragInfo = {
        state: DragState.Waiting,
        lastPos: defPos
    }

    return mkDragEndable(unwrap(loop(processDrag, def, e)))
}

export interface InputEvents {
    tapped: Stream<TapEvent>
    zoomed: Stream<WheelEvent>
    dragged: Stream<DragEvent>
}

export function setupInput(elem: Element): InputEvents {
    const mouseTap = (e: MouseEvent): TapEvent => {
        return {
            tapX: e.clientX,
            tapY: e.clientY
        }
    }
    const touchTap = (e: TouchEvent) => {
        const t = e.touches[0]
        return {
            tapX: t.clientX,
            tapY: t.clientY
        }
    }

    const mouseStart = map(mouseTap, mousedown(elem))
    const mouseMove = map(mouseTap, mousemove(elem))
    const mouseEnd = map(mouseTap, mouseup(elem))
    const touchStart = map(touchTap, touchstart(elem))
    const touchMove = map(touchTap, touchmove(elem))
    const touchEnd = map(touchTap, touchend(elem))

    const start = merge(mouseStart, touchStart)
    const move = merge(mouseMove, touchMove)
    const end = merge(mouseEnd, touchEnd)

    return {
        tapped: multicast(tapped(start, end)),
        zoomed: multicast(domEvent('wheel', elem)),
        dragged: multicast(dragged(start, move, end))
    }
}
