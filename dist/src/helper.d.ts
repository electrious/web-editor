import { Stream } from '@most/types';
/**
 * get the default scheduler used in stream processing. This function is
 * memoized so the scheduler is created once and shared.
 */
export declare const defScheduler: () => import("@most/types").Scheduler;
/**
 * unwrap values that can be null in a stream to non-null stream
 * @param src
 */
export declare function unwrap<T>(src: Stream<T | null | undefined>): Stream<T>;
/**
 * add a debug log to the stream
 * @param src
 */
export declare function debug<T>(src: Stream<T>): Stream<T>;
/**
 * Only propagate events in 'stream' when 'pred' is true.
 * @param pred
 * @param stream
 */
export declare function gate<T>(pred: Stream<boolean>, stream: Stream<T>): Stream<T>;
