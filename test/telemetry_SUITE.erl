-module(telemetry_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [bad_event_names, duplicate_attach, invoke_handler, default_metadata,
     list_handlers, list_for_prefix, detach_on_exception,
     no_execute_detached, no_execute_on_prefix, no_execute_on_specific,
     handler_on_multiple_events, remove_all_handler_on_failure,
     list_handler_on_many, detach_from_all].

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
    ?assertError(badarg, telemetry:attach(HandlerId, hello, fun ?MODULE:echo_event/4, [])).

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
    EventConfig = #{send_to => self()},
    Value = 1,
    Metadata = #{some => metadata},
    telemetry:attach(HandlerId, Event, fun ?MODULE:echo_event/4, EventConfig),

    telemetry:execute(Event, Value, Metadata),

    receive
        {event, Event, Value, Metadata, EventConfig} ->
            ok
    after
        1000 ->
            ct:fail(timeout_receive_echo)
    end.

%% event metadata is an empty map by default
default_metadata(Config) ->
    HandlerId = ?config(id, Config),
    Event = [a, test, event],
    EventConfig = #{send_to => self()},
    Value = 1,
    telemetry:attach(HandlerId, Event, fun ?MODULE:echo_event/4, EventConfig),

    telemetry:execute(Event, Value),

    receive
        {event, Event, Value, Metadata, EventConfig} ->
            ?assertEqual(#{}, Metadata)
    after
        1000 ->
            ct:fail(timeout_receive_echo)
    end.

%% handlers attached to event can be listed
list_handlers(Config) ->
    HandlerId = ?config(id, Config),
    Event = [a, test, event],
    EventConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:echo_event/4,
    telemetry:attach(HandlerId, Event, HandlerFun, EventConfig),

    ?assertMatch([#{id := HandlerId,
                    event_name := Event,
                    function := HandlerFun,
                    config := EventConfig}],
                 telemetry:list_handlers(Event)).

%% handlers attached to event prefix can be listed
list_for_prefix(Config) ->
    HandlerId = ?config(id, Config),
    Prefix1 = [],
    Prefix2 = [a],
    Prefix3 = [a, test],
    Event = [a, test, event],
    EventConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:echo_event/4,
    telemetry:attach(HandlerId, Event, HandlerFun, EventConfig),

    [?assertMatch([#{id := HandlerId,
                     event_name := Event,
                     function := HandlerFun,
                     config := EventConfig}],
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

    telemetry:execute(Event, 1),

    ?assertEqual([], telemetry:list_handlers(Event)),

    %% detaching returns error if handler with given ID doesn't exist
    ?assertEqual({error, not_found}, telemetry:detach(<<"doesn't exist">>)).

%% detached handler function is not called when handlers are executed
no_execute_detached(Config) ->
    HandlerId = ?config(id, Config),
    Event = [a, test, event],
    EventConfig = #{send_to => self()},
    Value = 1,
    Metadata = #{some => metadata},
    HandlerFun = fun ?MODULE:echo_event/4,
    telemetry:attach(HandlerId, Event, HandlerFun, EventConfig),
    telemetry:detach(HandlerId),
    telemetry:execute(Event, Value, Metadata),

    receive
        {event, Event, Value, Metadata, Config} ->
            ct:fail(prefix_executed)
    after
        300 ->
            ok
    end.

%% handler is not invoked when prefix of the event it's attached to is emitted
no_execute_on_prefix(Config) ->
    HandlerId = ?config(id, Config),
    Prefix = [a, test],
    Event = [a, test, event],
    EventConfig = #{send_to => self()},
    Value = 1,
    Metadata = #{some => metadata},
    HandlerFun = fun ?MODULE:echo_event/4,
    telemetry:attach(HandlerId, Event, HandlerFun, EventConfig),

    telemetry:execute(Prefix, Value, Metadata),

    receive
        {event, Event, Value, Metadata, Config} ->
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
    EventConfig = #{send_to => self()},
    Value = 1,
    Metadata = #{some => metadata},
    HandlerFun = fun ?MODULE:echo_event/4,
    telemetry:attach(HandlerId, Event, HandlerFun, EventConfig),

    telemetry:execute(MoreSpecificEvent, Value, Metadata),

    receive
        {event, Event, Value, Metadata, Config} ->
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
    EventConfig = #{send_to => self()},
    Value = 1,
    Metadata = #{some => metadata},
    HandlerFun = fun ?MODULE:echo_event/4,
    telemetry:attach_many(HandlerId, [Event1, Event2, Event3], HandlerFun, EventConfig),

    telemetry:execute(Event1, Value, Metadata),
    telemetry:execute(Event2, Value, Metadata),
    telemetry:execute(Event3, Value, Metadata),

    lists:foreach(fun(Event) ->
                          receive
                              {event, Event, Value, Metadata, EventConfig} ->
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
    EventConfig = #{send_to => self()},
    Value = 1,
    Metadata = #{some => metadata},
    HandlerFun = fun ?MODULE:raise_on_event/4,

    telemetry:attach_many(HandlerId, [Event1, Event2, Event3], HandlerFun, EventConfig),

    telemetry:execute(Event1, Value, Metadata),

    lists:foreach(fun(Event) ->
                          ?assertEqual([], telemetry:list_handlers(Event))
                  end, [Event1, Event2, Event3]).

%% handler attached to many events at once can be listed
list_handler_on_many(Config) ->
    HandlerId = ?config(id, Config),
    Event1 = [a, first, event],
    Event2 = [a, second, event],
    Event3 = [a, third, event],
    EventConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:echo_event/4,

    telemetry:attach_many(HandlerId, [Event1, Event2, Event3], HandlerFun, EventConfig),

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
    EventConfig = #{send_to => self()},
    HandlerFun = fun ?MODULE:echo_event/4,

    telemetry:attach_many(HandlerId, [Event1, Event2, Event3], HandlerFun, EventConfig),

    telemetry:detach(HandlerId),

    lists:foreach(fun(Event) ->
                          ?assertEqual([], telemetry:list_handlers(Event))
                  end, [Event1, Event2, Event3]).

%%

echo_event(Event, Value, Metadata, #{send_to := Pid}=Config) ->
    Pid ! {event, Event, Value, Metadata, Config}.

raise_on_event(_, _, _, _) ->
    throw(got_event).
