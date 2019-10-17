import { Stream } from '@most/types'
import { filter } from '@most/core'

/**
 * unwrap values that can be null in a stream to non-null stream
 * @param src
 */
export function unwrap<T>(src: Stream<T | null>): Stream<T> {
    return filter(v => v != null, src)
}
