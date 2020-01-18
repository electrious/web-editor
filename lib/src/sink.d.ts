import { Sink } from '@most/types';
/**
 * mkSink is a helper function to create a Sink without the need to specify all
 * required fields
 * @param event a callback to process value sent to the sink
 * @param error a callback to process error sent to the sink
 * @param end a callback to process end event sent to the sink
 * @returns Sink<T>
 */
export declare function mkSink<T>(event?: ((val: T) => void) | undefined, error?: ((err: Error) => void) | undefined, end?: (() => void) | undefined): Sink<T>;
