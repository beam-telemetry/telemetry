-module(telemetry_test_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [simple_message].

init_per_suite(Config) ->
    application:ensure_all_started(telemetry),
    Config.

end_per_suite(_Config) ->
    application:stop(telemetry).

init_per_testcase(_, Config) ->
    HandlerId = crypto:strong_rand_bytes(16),
    [{id, HandlerId} | Config].

end_per_testcase(_, Config) ->
    HandlerId = ?config(id, Config),
    telemetry:detach(HandlerId).

%% attach_message_handlers/3 works correctly.
simple_message(Config) ->
    HandlerId = ?config(id, Config),
    Ref = telemetry_test:attach_message_handlers(HandlerId, self(), [[some, event]]),

    telemetry:execute([some, event], #{measurement => 1}, #{meta => 2}),

    receive
        {[some, event], Ref, #{measurement := 1}, #{meta := 2}} ->
            ok
    after 1000 ->
        ct:fail(timeout_receive_attach_message_handlers)
    end.
