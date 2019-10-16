import { Sink } from '@most/types'

/**
 * mkSink is a helper function to create a Sink without the need to specify all
 * required fields
 * @param event a callback to process value sent to the sink
 * @param error a callback to process error sent to the sink
 * @param end a callback to process end event sent to the sink
 * @returns Sink<T>
 */
export function mkSink<T>(
    event: (val: T) => void,
    error?: ((err: Error) => void) | undefined,
    end?: (() => void) | undefined
): Sink<T> {
    return {
        event: (_t, v) => {
            event(v)
        },
        error: (_t, e) => {
            if (error != undefined) {
                error(e)
            }
        },
        end: () => {
            if (end != undefined) {
                end()
            }
        }
    }
}
