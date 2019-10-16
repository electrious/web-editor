import { Stream } from '@most/types'
import { empty } from '@most/core'
import { click, touchstart, touchend } from '@most/dom-event'

export interface InputEvents {
    clicked: Stream<Event>
    tapped: Stream<Event>
}

export function setupInput(elem: Element): InputEvents {
    const clicked = click(elem)
    const touchS = touchstart(elem)
    const touchE = touchend(elem)

    return {
        clicked: clicked,
        tapped: empty()
    }
}
