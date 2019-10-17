import { Stream } from '@most/types'
import { empty, scan, merge, map, delay, combine, filter } from '@most/core'
import {
    click,
    domEvent,
    mousedown,
    mousemove,
    mouseup,
    touchstart,
    touchend
} from '@most/dom-event'
import { always, and } from 'ramda'
import { unwrap } from './helper'

export interface TapEvent {
    tapX: number
    tapY: number
}

/**
 * Tap gesture recognizer for touches
 * @param elem
 */
function tapped(elem: Element): Stream<TapEvent> {
    const start = touchstart(elem)
    const end = touchend(elem)

    const canBeTap = scan(
        and,
        false,
        merge(map(always(true), start), map(always(false), end))
    )
    // the touch should end in less than 0.5 seconds to be considered a tap.
    const tapCheckEvt = delay(500, start)
    const tapEvt = unwrap(
        combine(
            (can: boolean, chk) => {
                if (can) return chk
                else return null
            },
            canBeTap,
            tapCheckEvt
        )
    )

    return map(t => {
        const touch = t.touches[0]
        return {
            tapX: touch.clientX,
            tapY: touch.clientY
        }
    }, tapEvt)
}

export interface InputEvents {
    clicked: Stream<MouseEvent>
    tapped: Stream<TapEvent>
    zoomed: Stream<WheelEvent>
    dragged: Stream<DragEvent>
}

export function setupInput(elem: Element): InputEvents {
    return {
        clicked: click(elem),
        tapped: tapped(elem),
        zoomed: domEvent('wheel', elem),
        dragged: empty()
    }
}
