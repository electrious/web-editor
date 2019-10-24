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
    const doGate = (pred: boolean, v: T) => {
        return pred ? v : null
    }
    return unwrap(snapshot(doGate, pred, stream))
}

/**
 * propagate value in 'stream' when events in 'sampler' occurs.
 * @param stream
 * @param sampler
 */
export function tag<T, S>(stream: Stream<T>, sampler: Stream<S>): Stream<T> {
    const f = (v1: T, _v2: S) => v1

    return snapshot(f, stream, sampler)
}
