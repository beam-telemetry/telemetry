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
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% attach_event_handlers/2 works correctly.
simple_message(_Config) ->
    Ref = telemetry_test:attach_event_handlers(self(), [[some, event]]),

    telemetry:execute([some, event], #{measurement => 1}, #{meta => 2}),

    receive
        {[some, event], Ref, #{measurement := 1}, #{meta := 2}} ->
            telemetry:detach(Ref)
    after 1000 ->
        ct:fail(timeout_receive_attach_event_handlers)
    end.
