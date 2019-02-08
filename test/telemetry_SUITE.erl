-module(telemetry_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [bad_event_names, duplicate_attach, invoke_handler,
     list_handlers, list_for_prefix, detach_on_exception,
     no_execute_detached, no_execute_on_prefix, no_execute_on_specific,
     handler_on_multiple_events, remove_all_handler_on_failure,
     list_handler_on_many, detach_from_all, old_execute, default_metadata].

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

%% handler function is detached when it fails
detach_on_exception(Config) ->
    HandlerId = ?config(id, Config),
    Event = [a, test, event],
    HandlerFun = fun ?MODULE:raise_on_event/4,
    telemetry:attach(HandlerId, Event, HandlerFun, []),

    ?assertMatch([#{id := HandlerId,
                    event_name := Event,
                    function := HandlerFun,
                    config := []}],
                 telemetry:list_handlers(Event)),

    telemetry:execute(Event, #{some => 1}, #{some => metadata}),

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

%% handler attached to many events at once is detached on failure of any invokation
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
                                          config := EventConfig}],
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


echo_event(Event, Measurements, Metadata, #{send_to := Pid} = Config) ->
    Pid ! {event, Event, Measurements, Metadata, Config}.

raise_on_event(_, _, _, _) ->
    throw(got_event).
