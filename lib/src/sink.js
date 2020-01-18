"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/**
 * mkSink is a helper function to create a Sink without the need to specify all
 * required fields
 * @param event a callback to process value sent to the sink
 * @param error a callback to process error sent to the sink
 * @param end a callback to process end event sent to the sink
 * @returns Sink<T>
 */
function mkSink(event, error, end) {
    return {
        event: function (_t, v) {
            if (event != undefined) {
                event(v);
            }
        },
        error: function (_t, e) {
            if (error != undefined) {
                error(e);
            }
        },
        end: function () {
            if (end != undefined) {
                end();
            }
        }
    };
}
exports.mkSink = mkSink;
//# sourceMappingURL=sink.js.map