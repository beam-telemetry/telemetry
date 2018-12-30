-record(handler, {id :: telemetry:handler_id(),
                  event_name :: telemetry:event_name() | '_',
                  function   :: telemetry:handler_function(),
                  config     :: telemetry:handler_config() | '_'}).

-ifdef('OTP_RELEASE').
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-else.
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(),).
-endif.
