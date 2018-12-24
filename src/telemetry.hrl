-record(handler, {handler_id :: telemetry:handler_id(),
                  event_name :: telemetry:event_name() | '_',
                  function   :: telemetry:handler_function(),
                  config     :: term() | '_'}).
