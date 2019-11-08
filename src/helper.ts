import { Stream } from '@most/types'
import { filter, tap, snapshot } from '@most/core'
import memoizeWith from 'ramda/es/memoizeWith'
import always from 'ramda/es/always'
import { newDefaultScheduler } from '@most/scheduler'

/**
 * get the default scheduler used in stream processing. This function is
 * memoized so the scheduler is created once and shared.
 */
export const defScheduler = memoizeWith(always('def_scheduler'), () => {
    return newDefaultScheduler()
})

/**
 * unwrap values that can be null in a stream to non-null stream
 * @param src
 */
export function unwrap<T>(src: Stream<T | null | undefined>): Stream<T> {
    return filter(v => v != null && v != undefined, src)
}

/**
 * add a debug log to the stream
 * @param src
 */
export function debug<T>(src: Stream<T>): Stream<T> {
    return tap(v => console.log(v), src)
}

/**
 * Only propagate events in 'stream' when 'pred' is true.
 * @param pred
 * @param stream
 */
export function gate<T>(pred: Stream<boolean>, stream: Stream<T>): Stream<T> {
    return unwrap(snapshot((pred, v) => (pred ? v : null), pred, stream))
}
