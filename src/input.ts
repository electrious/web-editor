import { Stream, Disposable } from '@most/types'
import {
    merge,
    map,
    delay,
    startWith,
    mergeArray,
    constant,
    multicast,
    debounce,
    skip,
    scan,
    snapshot,
    skipRepeats,
    filter,
    sample
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
import { gate, unwrap, defScheduler, debug } from './helper'
import not from 'ramda/es/not'
import { createAdapter } from '@most/adapter'
import { mkSink } from './sink'

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

/**
 * MouseMoveEvent encode the mouse position for MouseMove event.
 */
export interface MouseMoveEvent {
    mouseX: number
    mouseY: number
}

function mouseMoveEvent(e: MouseEvent): MouseMoveEvent {
    return {
        mouseX: e.offsetX,
        mouseY: e.offsetY
    }
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

const isEnd = (e: DragEvent) => {
    return e.dragType == DragType.DragEnd
}

const updateDragType = curry(
    (t: DragType, d: DragEvent): DragEvent => {
        return {
            dragType: t,
            dragX: d.dragX,
            dragY: d.dragY,
            deltaX: d.deltaX,
            deltaY: d.deltaY
        }
    }
)

function distance(d1: DragEvent | null, d2: DragEvent): number {
    if (d1 == null) return 0

    const dx = d1.dragX - d2.dragX
    const dy = d1.dragY - d2.dragY
    return Math.sqrt(dx * dx + dy * dy)
}

function mkDragEndable(evt: Stream<DragEvent>): Stream<DragEvent> {
    // wait for 2 seconds and see if there're new events
    // if not, make sure the last one is DragEnd
    const e = debounce(1500, evt)

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
): [Stream<DragEvent>, Disposable] {
    const startDrag = constant(true, start)
    const endDrag = constant(false, end)

    // create an adapter stream for auto end event
    const [updateEnd, possibleEnd] = createAdapter()
    const posEnd = constant(false, possibleEnd)

    // we're only interested in move events between start and end
    const touching = startWith(false, mergeArray([startDrag, endDrag, posEnd]))

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

    // only move events that are in between 'touching' is real move
    const realMove = map(mkDrag(DragType.Drag), gate(touching, move))

    // make sure user did move the mouse/touch
    const checkDist = (s: DragEvent | null, p: DragEvent) => {
        return distance(s, p) >= 1 ? p : null
    }

    const dstart = map(mkDrag(DragType.DragStart), start)
    const startPos = startWith(null, dstart)
    const dragMove = unwrap(snapshot(checkDist, startPos, realMove))

    // when user is actually dragging
    const dragging = skipRepeats(
        startWith(
            false,
            mergeArray([
                constant(false, start),
                constant(true, dragMove),
                constant(false, end)
            ])
        )
    )
    const notDragging = map(not, dragging)
    // the drag start should be the first drag event attached with start position
    const dragStart = unwrap(sample(startPos, gate(notDragging, dragMove)))

    // calculate the new drag end event
    const lastPos = startWith(null, dragMove)
    const lastDrag = unwrap(sample(lastPos, gate(dragging, end)))
    const dragEnd = map(updateDragType(DragType.DragEnd), lastDrag)

    const def: DragEvent = {
        dragType: DragType.DragStart,
        dragX: 0,
        dragY: 0,
        deltaX: 0,
        deltaY: 0
    }

    // merge all drag related events and do delta calculation
    const evts = mergeArray([dragStart, dragMove, dragEnd])

    const calcDelta = (lastE: DragEvent, e: DragEvent): DragEvent => {
        if (e.dragType == DragType.DragStart) {
            return e
        } else {
            e.deltaX = e.dragX - lastE.dragX
            e.deltaY = e.dragY - lastE.dragY
            return e
        }
    }

    const resEvt = mkDragEndable(skip(1, scan(calcDelta, def, evts)))

    // filter End event and pipe it back to the internal possibleEnd stream
    const d = filter(isEnd, resEvt).run(mkSink(updateEnd), defScheduler())

    return [resEvt, d]
}

export interface InputEvents {
    tapped: Stream<TapEvent>
    zoomed: Stream<WheelEvent>
    dragged: Stream<DragEvent>
    mouseMove: Stream<MouseMoveEvent>
    disposable: Disposable
}

/**
 * Setup the input system for an element. It will return the InputEvents object
 * with all supported event streams.
 * @param elem
 */
export function setupInput(elem: Element): InputEvents {
    const mouseTap = (e: MouseEvent): TapEvent => {
        return {
            tapX: e.offsetX,
            tapY: e.offsetY
        }
    }

    const touchTap = (e: TouchEvent) => {
        const t = e.touches[0]
        const rect = elem.getBoundingClientRect()
        return {
            tapX: t.pageX - rect.left,
            tapY: t.clientY - rect.top
        }
    }

    const mouseStart = map(mouseTap, mousedown(elem))
    const mouseMove = map(mouseTap, mousemove(elem))
    const mouseEnd = map(mouseTap, mouseup(elem))
    const touchStart = map(touchTap, touchstart(elem))
    const touchMove = map(touchTap, touchmove(elem))
    const touchEnd = map(touchTap, touchend(elem))

    const start = multicast(merge(mouseStart, touchStart))
    const move = multicast(merge(mouseMove, touchMove))
    const end = multicast(merge(mouseEnd, touchEnd))

    const [drag, disp] = dragged(start, move, end)

    return {
        tapped: multicast(tapped(start, end)),
        zoomed: multicast(domEvent('wheel', elem)),
        dragged: multicast(drag),
        mouseMove: multicast(map(mouseMoveEvent, mousemove(elem))),
        disposable: disp
    }
}
