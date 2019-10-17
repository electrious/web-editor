import { Stream } from '@most/types'
import { map, empty, fromPromise } from '@most/core'
import { click, domEvent } from '@most/dom-event'

export interface TapEvent {
    tapX: number
    tapY: number
}

export interface InputEvents {
    clicked: Stream<MouseEvent>
    tapped: Stream<TapEvent>
    zoomed: Stream<WheelEvent>
}

export function setupInput(elem: Element): InputEvents {
    return {
        clicked: click(elem),
        tapped: empty(),
        zoomed: domEvent('wheel', elem)
    }
}
