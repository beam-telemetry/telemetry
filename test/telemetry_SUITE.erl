-module(telemetry_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("telemetry.hrl").

all() ->
    [bad_event_names, duplicate_attach, invoke_handler,
     list_handlers, list_for_prefix, detach_on_exception,
     no_execute_detached, no_execute_on_prefix, no_execute_on_specific,
     handler_on_multiple_events, remove_all_handler_on_failure,
     list_handler_on_many, detach_from_all, old_execute, default_metadata,
     off_execute, invoke_successful_span_handlers, invoke_exception_span_handlers,
     spans_generate_unique_default_contexts, logs_on_local_function].

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

bad_event_names(Config) ->
    HandlerId = ?config(id, Config),
    ?assertError(badarg, telemetry:attach(HandlerId, ["some", event], fun ?MODULE:echo_event/4, [])),
    ?assertError(badarg, telemetry:attach(HandlerId, hello, fun ?MODULE:echo_event/4, [])),
    ?assertError(badarg, telemetry:attach(HandlerId, [], fun ?MODULE:echo_event/4, [])).

%% attaching returns error if handler with the same ID already exist
duplicate_attach(Config) ->
    HandlerId = ?config(id, Config),
    telemetry:attach(HandlerId, [some, event], fun ?MODULE:echo_event/4, []),

    ?assertEqual({error, already_exists},
                 telemetry:attach(HandlerId, [some, event], fun ?MODULE:echo_event/4, [])).

%% handler is invoked when event it's attached to is emitted
invoke_handler(Config) ->
    HandlerId = ?config(id, Config),
    Event = [a, test, event],
    HandlerConfig = #{send_to => self()},
    Measurements = #{data => 3},
    Metadata = #{some => metadata},
    telemetry:attach(HandlerId, Event, fun ?MODULE:echo_event/4, HandlerConfig),

    telemetry:execute(Event, Measurements, Metadata),

    receive
        {event, Event, Measurements, Metadata, HandlerConfig} ->
            ok
    after
        1000 ->
            ct:fail(timeout_receive_echo)
    end.

%% handlers attached to event can be listed
list_handlers(Config) ->
    HandlerId = ?config(id, Config),
    Event = [a, test, event],
    HandlerConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:echo_event/4,
    telemetry:attach(HandlerId, Event, HandlerFun, HandlerConfig),

    ?assertMatch([#{id := HandlerId,
                    event_name := Event,
                    function := HandlerFun,
                    config := HandlerConfig}],
                 telemetry:list_handlers(Event)).

%% handlers attached to event prefix can be listed
list_for_prefix(Config) ->
    HandlerId = ?config(id, Config),
    Prefix1 = [],
    Prefix2 = [a],
    Prefix3 = [a, test],
    Event = [a, test, event],
    HandlerConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:echo_event/4,
    telemetry:attach(HandlerId, Event, HandlerFun, HandlerConfig),

    [?assertMatch([#{id := HandlerId,
                     event_name := Event,
                     function := HandlerFun,
                     config := HandlerConfig}],
                  telemetry:list_handlers(Prefix)) || Prefix <- [Prefix1, Prefix2, Prefix3]],

     ?assertEqual([], telemetry:list_handlers(Event ++ [something])).

%% handler function is detached when it fails and failure event is emitted
detach_on_exception(Config) ->
    HandlerId = ?config(id, Config),
    Event = [a, test, event],
    HandlerFun = fun ?MODULE:raise_on_event/4,
    HandlerConfig = [],

    FailureHandlerId = failure_handler_id,
    FailureEvent = [telemetry, handler, failure],
    FailureHandlerConfig = #{send_to => self()},
    FailureHandlerFun = fun ?MODULE:echo_event/4,

    telemetry:attach(HandlerId, Event, HandlerFun, HandlerConfig),
    telemetry:attach(FailureHandlerId, FailureEvent, FailureHandlerFun, FailureHandlerConfig),

    ?assertMatch([#{id := HandlerId,
                    event_name := Event,
                    function := HandlerFun,
                    config := HandlerConfig}],
                 telemetry:list_handlers(Event)),

    telemetry:execute(Event, #{some => 1}, #{some => metadata}),

    receive
        {event, FailureEvent, _FailureMeasurements, FailureMetadata, FailureHandlerConfig} ->
            ?assertMatch(#{event_name := Event,
                           handler_id := HandlerId,
                           handler_config := HandlerConfig,
                           kind := throw,
                           reason := got_event,
                           stacktrace := [_ | _]},
                         FailureMetadata),
            ok
    after
        300 ->
            ct:fail(failure_event_not_emitted)
    end,

    ?assertEqual([], telemetry:list_handlers(Event)),

    %% detaching returns error if handler with given ID doesn't exist
    ?assertEqual({error, not_found}, telemetry:detach(<<"doesn't exist">>)).

%% detached handler function is not called when handlers are executed
no_execute_detached(Config) ->
    HandlerId = ?config(id, Config),
    Event = [a, test, event],
    HandlerConfig = #{send_to => self()},
    Measurements = #{data => 3},
    Metadata = #{some => data},
    HandlerFun = fun ?MODULE:echo_event/4,
    telemetry:attach(HandlerId, Event, HandlerFun, HandlerConfig),
    telemetry:detach(HandlerId),
    telemetry:execute(Event, Measurements, Metadata),

    receive
        {event, Event, Measurements, Metadata, HandlerConfig} ->
            ct:fail(detached_executed)
    after
        300 ->
            ok
    end.

%% handler is not invoked when prefix of the event it's attached to is emitted
no_execute_on_prefix(Config) ->
    HandlerId = ?config(id, Config),
    Prefix = [a, test],
    Event = [a, test, event],
    Measurements = #{data => 3},
    Metadata = #{some => data},
    HandlerConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:echo_event/4,
    telemetry:attach(HandlerId, Event, HandlerFun, HandlerConfig),

    telemetry:execute(Prefix, Measurements, Metadata),

    receive
        {event, Event, Measurements, Metadata, HandlerConfig} ->
            ct:fail(prefix_executed)
    after
        300 ->
            ok
    end.

%% handler is not invoked when event more specific than the one it's attached to is emitted
no_execute_on_specific(Config) ->
    HandlerId = ?config(id, Config),
    Event = [a, test],
    MoreSpecificEvent = [a, test, event, specific],
    Measurements = #{data => 3},
    Metadata = #{some => data},
    HandlerConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:echo_event/4,
    telemetry:attach(HandlerId, Event, HandlerFun, HandlerConfig),

    telemetry:execute(MoreSpecificEvent, Measurements, Metadata),

    receive
        {event, Event, Measurements, Metadata, HandlerConfig} ->
            ct:fail(specific_executed)
    after
        300 ->
            ok
    end.

%% handler can be attached to many events at once
handler_on_multiple_events(Config) ->
    HandlerId = ?config(id, Config),
    Event1 = [a, first, event],
    Event2 = [a, second, event],
    Event3 = [a, third, event],
    Measurements = #{data => 3},
    Metadata = #{some => data},
    HandlerConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:echo_event/4,
    telemetry:attach_many(HandlerId, [Event1, Event2, Event3], HandlerFun, HandlerConfig),

    telemetry:execute(Event1, Measurements, Metadata),
    telemetry:execute(Event2, Measurements, Metadata),
    telemetry:execute(Event3, Measurements, Metadata),

    lists:foreach(fun(Event) ->
                          receive
                              {event, Event, Measurements, Metadata, HandlerConfig} ->
                                  ok
                          after
                              300 ->
                                  ct:fail(missing_echo_event)
                          end
                  end, [Event1, Event2, Event3]).

%% handler attached to many events at once is detached on failure of any invocation
remove_all_handler_on_failure(Config) ->
    HandlerId = ?config(id, Config),
    Event1 = [a, first, event],
    Event2 = [a, second, event],
    Event3 = [a, third, event],
    Measurements = #{data => 3},
    Metadata = #{some => data},
    HandlerConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:raise_on_event/4,

    telemetry:attach_many(HandlerId, [Event1, Event2, Event3], HandlerFun, HandlerConfig),

    telemetry:execute(Event1, Measurements, Metadata),

    lists:foreach(fun(Event) ->
                          ?assertEqual([], telemetry:list_handlers(Event))
                  end, [Event1, Event2, Event3]).

%% handler attached to many events at once can be listed
list_handler_on_many(Config) ->
    HandlerId = ?config(id, Config),
    Event1 = [a, first, event],
    Event2 = [a, second, event],
    Event3 = [a, third, event],
    HandlerConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:echo_event/4,

    telemetry:attach_many(HandlerId, [Event1, Event2, Event3], HandlerFun, HandlerConfig),

    lists:foreach(fun(Event) ->
                          ?assertMatch([#{id := HandlerId,
                                          event_name := Event,
                                          function := HandlerFun,
                                          config := _EventConfig}],
                                       telemetry:list_handlers(Event))
    end, [Event1, Event2, Event3]).

%% handler attached to many events at once is detached from all of them
detach_from_all(Config) ->
    HandlerId = ?config(id, Config),
    Event1 = [a, first, event],
    Event2 = [a, second, event],
    Event3 = [a, third, event],
    HandlerConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:echo_event/4,

    telemetry:attach_many(HandlerId, [Event1, Event2, Event3], HandlerFun, HandlerConfig),

    telemetry:detach(HandlerId),

    lists:foreach(fun(Event) ->
                          ?assertEqual([], telemetry:list_handlers(Event))
                  end, [Event1, Event2, Event3]).

old_execute(Config) ->
    HandlerId = ?config(id, Config),
    Event = [a, first, event],
    Value = 1,
    Metadata = #{some => metadata},
    HandlerConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:echo_event/4,

    telemetry:attach(HandlerId, Event, HandlerFun, HandlerConfig),
    telemetry:execute(Event, Value, Metadata),

    receive
        {event, Event, Measurements, Metadata, HandlerConfig} ->
            ?assertEqual(#{value => Value}, Measurements)
    after
        1000 ->
            ct:fail(missing_echo_event)
    end.

default_metadata(Config) ->
    HandlerId = ?config(id, Config),
    Event = [a, first, event],
    Measurements = #{data => 3},
    HandlerConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:echo_event/4,

    telemetry:attach(HandlerId, Event, HandlerFun, HandlerConfig),
    telemetry:execute(Event, Measurements),

    receive
        {event, Event, Measurements, Metadata, HandlerConfig} ->
            ?assertEqual(0, map_size(Metadata))
    after
        1000 ->
            ct:fail(missing_echo_event)
    end.

% Ensure that a start and stop event are emitted during a successful span call
invoke_successful_span_handlers(Config) ->
    HandlerId = ?config(id, Config),
    EventPrefix = [some, action],
    StartEvent = EventPrefix ++ [start],
    StopEvent = EventPrefix ++ [stop],
    HandlerConfig = #{send_to => self()},
    StartMetadata = #{some => start_metadata, telemetry_span_context => ctx},
    StopMetadata = #{other => stop_metadata, telemetry_span_context => ctx},
    ErrorSpanFunction = fun() -> {ok, StopMetadata} end,

    telemetry:attach_many(HandlerId, [StartEvent, StopEvent], fun ?MODULE:echo_event/4, HandlerConfig),
    telemetry:span(EventPrefix, StartMetadata, ErrorSpanFunction),

    receive
        {event, StartEvent, StartMeasurements, StartMetadata, HandlerConfig} ->
          ?assertEqual([monotonic_time, system_time], lists:sort(maps:keys(StartMeasurements)))
    after
        1000 -> ct:fail(timeout_receive_echo)
    end,

    receive
        {event, StopEvent, StopMeasurements, StopMetadata, HandlerConfig} ->
          ?assertEqual([duration, monotonic_time], lists:sort(maps:keys(StopMeasurements)))
    after
        1000 -> ct:fail(timeout_receive_echo)
    end.

% Ensure that stop event includes custom measurements if provided
invoke_successful_span_handlers_with_measurements(Config) ->
    HandlerId = ?config(id, Config),
    EventPrefix = [some, action],
    StartEvent = EventPrefix ++ [start],
    StopEvent = EventPrefix ++ [stop],
    HandlerConfig = #{send_to => self()},
    StartMetadata = #{some => start_metadata, telemetry_span_context => ctx},
    StopMetadata = #{other => stop_metadata, telemetry_span_context => ctx},
    ExtraMeasurements = #{other_thing => 100},
    ErrorSpanFunction = fun() -> {ok, ExtraMeasurements, StopMetadata} end,

    telemetry:attach_many(HandlerId, [StartEvent, StopEvent], fun ?MODULE:echo_event/4, HandlerConfig),
    telemetry:span(EventPrefix, StartMetadata, ErrorSpanFunction),

    receive
        {event, StartEvent, StartMeasurements, StartMetadata, HandlerConfig} ->
          ?assertEqual([monotonic_time, system_time], lists:sort(maps:keys(StartMeasurements)))
    after
        1000 -> ct:fail(timeout_receive_echo)
    end,

    receive
        {event, StopEvent, StopMeasurements, StopMetadata, HandlerConfig} ->
          ?assertEqual([duration, monotonic_time, other_thing], lists:sort(maps:keys(StopMeasurements)))
    after
        1000 -> ct:fail(timeout_receive_echo)
    end.

% Ensure that a start and exception event are emitted during an error span call
invoke_exception_span_handlers(Config) ->
    HandlerId = ?config(id, Config),
    EventPrefix = [some, action],
    StartEvent = EventPrefix ++ [start],
    ExceptionEvent = EventPrefix ++ [exception],
    HandlerConfig = #{send_to => self()},
    StartMetadata = #{some => start_metadata, telemetry_span_context => ctx},
    SpanFunction = fun() -> erlang:error(badarith) end,

    telemetry:attach_many(HandlerId, [StartEvent, ExceptionEvent], fun ?MODULE:echo_event/4, HandlerConfig),

    try
        telemetry:span(EventPrefix, StartMetadata, SpanFunction),
        ct:fail(span_function_expected_error)
    catch
        ?WITH_STACKTRACE(Class, Reason, Stacktrace)
            ?assertEqual(error, Class),
            ?assertEqual(badarith, Reason),
            ?assert(erlang:is_list(Stacktrace))
    end,

    receive
        {event, StartEvent, StartMeasurements, StartMetadata, HandlerConfig} ->
          ?assertEqual([monotonic_time, system_time], lists:sort(maps:keys(StartMeasurements)))
    after
        1000 -> ct:fail(timeout_receive_echo)
    end,

    receive
        {event, ExceptionEvent, StopMeasurements, ExceptionMetadata, HandlerConfig} ->
          ?assertEqual([duration, monotonic_time], lists:sort(maps:keys(StopMeasurements))),
          ?assertEqual([kind, reason, some, stacktrace, telemetry_span_context], lists:sort(maps:keys(ExceptionMetadata)))
    after
        1000 -> ct:fail(timeout_receive_echo)
    end.

% Default span context is generated with a unique value per invocation within a process
spans_generate_unique_default_contexts(Config) ->
    HandlerId = ?config(id, Config),
    EventPrefix = [some, action],
    StartEvent = EventPrefix ++ [start],
    StopEvent = EventPrefix ++ [stop],
    HandlerConfig = #{send_to => self()},
    StartMetadata = #{},
    StopMetadata = #{},
    ErrorSpanFunction = fun() -> {ok, StopMetadata} end,

    telemetry:attach_many(HandlerId, [StartEvent, StopEvent], fun ?MODULE:echo_event/4, HandlerConfig),
    telemetry:span(EventPrefix, StartMetadata, ErrorSpanFunction),

    receive
        {event, StartEvent, _, FirstMetadata, HandlerConfig} ->
            FirstContext = maps:get(telemetry_span_context, FirstMetadata),
            ?assert(erlang:is_reference(FirstContext)),

            receive
                {event, StopEvent, _, #{telemetry_span_context:=FirstContext}, HandlerConfig} ->
                    telemetry:span(EventPrefix, StartMetadata, ErrorSpanFunction),

                    receive
                        {event, StartEvent, _, SecondMetadata, HandlerConfig} ->
                            SecondContext = maps:get(telemetry_span_context, SecondMetadata),
                            ?assertNotEqual(FirstContext, SecondContext),
                            ok
                    after
                        1000 -> ct:fail(timeout_receive_echo)
                    end,
                    ok
            after
                1000 -> ct:fail(timeout_receive_echo)
            end,

            ok
    after
        1000 -> ct:fail(timeout_receive_echo)
    end.

logs_on_local_function(Config) ->
    HandlerId = ?config(id, Config),
    Event = [some, action],

    OldConfig = logger:get_primary_config(),
    logger:add_primary_filter(logs_on_local_function, {fun ?MODULE:send_logs/2,
                                                       self()}),
    logger:update_primary_config(#{level => info}),

    try
        telemetry:attach(HandlerId, Event, fun raise_on_event/4, []),
        receive
            {log, #{msg := {report, #{handler_id := HandlerId}}}} -> ok
        after
            1000 -> ct:fail(timeout_receive_log)
        end,

        Fun = fun(_Event, _Measurements, _Meta, _Config) -> ok end,
        telemetry:attach(HandlerId, Event, Fun, []),
        receive
            {log, #{msg := {report, #{handler_id := HandlerId}}}} -> ok
        after
            1000 -> ct:fail(timeout_receive_log)
        end
    after
        logger:set_primary_config(OldConfig)
    end.

% Ensure calling execute is safe when the telemetry application is off
off_execute(_Config) ->
    application:stop(telemetry),
    telemetry:execute([event, name], #{}, #{}),
    application:ensure_all_started(telemetry).

echo_event(Event, Measurements, Metadata, #{send_to := Pid} = Config) ->
    Pid ! {event, Event, Measurements, Metadata, Config}.

raise_on_event(_, _, _, _) ->
    throw(got_event).

send_logs(Event, Pid) ->
    Pid ! {log, Event},
    ignore.
