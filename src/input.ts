import { Stream } from '@most/types'
import { merge, map, delay, startWith, mergeArray } from '@most/core'
import {
    click,
    domEvent,
    mousedown,
    mousemove,
    mouseup,
    touchstart,
    touchend,
    touchmove
} from '@most/dom-event'
import { always, curry } from 'ramda'
import { gate } from './helper'

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
        merge(map(always(true), start), map(always(false), end))
    )
    // the touch should end in less than 0.5 seconds to be considered a tap.
    const tapCheckEvt = delay(500, start)
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
    const startDrag = map(always(true), start)
    const endDrag = map(always(false), end)

    // we're only interested in move events between start and end
    const dragging = startWith(false, merge(startDrag, endDrag))
    const realMove = gate(dragging, move)

    const mkDrag = curry(
        (t: DragType, e: TapEvent): DragEvent => {
            return {
                dragType: t,
                dragX: e.tapX,
                dragY: e.tapY
            }
        }
    )

    const dstart = map(mkDrag(DragType.DragStart), start)
    const dend = map(mkDrag(DragType.DragEnd), end)
    const d = map(mkDrag(DragType.Drag), realMove)

    return mergeArray([dstart, d, dend])
}

export interface InputEvents {
    clicked: Stream<MouseEvent>
    tapped: Stream<TapEvent>
    zoomed: Stream<WheelEvent>
    dragged: Stream<DragEvent>
}

export function setupInput(elem: Element): InputEvents {
    const mouseTap = (e: MouseEvent) => {
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
        clicked: click(elem),
        tapped: tapped(start, end),
        zoomed: domEvent('wheel', elem),
        dragged: dragged(start, move, end)
    }
}
