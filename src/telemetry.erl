%%%-------------------------------------------------------------------
%% @doc `telemetry' allows you to invoke certain functions whenever a
%% particular event is emitted.
%%
%% For more information see the documentation for {@link attach/4}, {@link attach_many/4}
%% and {@link execute/3}.
%% @end
%%%-------------------------------------------------------------------
-module(telemetry).

-export([attach/4,
         attach_many/4,
         detach/1,
         list_handlers/1,
         execute/2,
         execute/3]).

-include("telemetry.hrl").

-ifdef('OTP_RELEASE').
-include_lib("kernel/include/logger.hrl").
-else.
-define(LOG_ERROR(Msg, Args), error_logger:error_msg(Msg, Args)).
-endif.


-type handler_id() :: term().
-type event_name() :: [atom()].
-type event_value() :: number().
-type event_metadata() :: map().
-type event_prefix() :: [atom()].
-type handler_config() :: term().
-type handler_function() :: fun((event_name(), event_value(), event_metadata(), handler_config()) -> ok).

%% TODO: change to := when OTP-18 support can be dropped
-type handler() :: #{id => handler_id(),
                     event_name => event_name(),
                     function => handler_function(),
                     config => handler_config()}.

-export_type([handler_id/0,
              event_name/0,
              event_value/0,
              event_metadata/0,
              event_prefix/0]).

%% @doc Attaches the handler to the event.
%%
%% `handler_id' must be unique, if another handler with the same ID already exists the
%% `{error, already_exists}' tuple is returned.
%% See {@link execute/3} to learn how the handlers are invoked.
-spec attach(HandlerId, EventName, Function, Config) -> ok | {error, already_exists} when
      HandlerId :: handler_id(),
      EventName :: event_name(),
      Function :: handler_function(),
      Config :: handler_config().
attach(HandlerId, EventName, Function, Config) ->
    attach_many(HandlerId, [EventName], Function, Config).

%% @doc Attaches the handler to many events.
%%
%% The handler will be invoked whenever any of the events in the `event_names' list is emitted. Note
%% that failure of the handler on any of these invokations will detach it from all the events in
%% `event_name' (the same applies to manual detaching using {@link detach/1}).
-spec attach_many(HandlerId, [EventName], Function, Config) -> ok | {error, already_exists} when
      HandlerId :: handler_id(),
      EventName :: event_name(),
      Function :: handler_function(),
      Config :: handler_config().
attach_many(HandlerId, EventNames, Function, Config) ->
    assert_event_names_or_prefixes(EventNames),
    telemetry_handler_table:insert(HandlerId, EventNames, Function, Config).

%% @doc Removes the existing handler.
%%
%% If the handler with given ID doesn't exist, `{error, not_found}' is returned.
-spec detach(handler_id()) -> ok | {error, not_found}.
detach(HandlerId) ->
    telemetry_handler_table:delete(HandlerId).

%% @equiv execute(EventName, EventValue, #{})
-spec execute(EventName, EventValue) -> ok when
      EventName :: event_name(),
      EventValue :: event_value().
execute(EventName, EventValue) ->
    execute(EventName, EventValue, #{}).

%% @doc Emits the event, invoking handlers attached to it.
%%
%% When the event is emitted, the handler function provided to {@link attach/4} is called with four
%% arguments:
%% <ul>
%% <li>the event name</li>
%% <li>the event value</li>
%% <li>the event metadata</li>
%% <li>the handler configuration given to {@link attach/4}</li>
%% </ul>
%% All the handlers are executed by the process calling this function. If the function fails (raises,
%% exits or throws) then the handler is removed.
%% Note that you should not rely on the order in which handlers are invoked.
-spec execute(EventName, EventValue, EventMetadata) -> ok when
      EventName :: event_name(),
      EventValue :: event_value(),
      EventMetadata :: event_metadata().
execute(EventName, EventValue, EventMetadata) when is_number(EventValue) ,
                                                   is_map(EventMetadata) ->
    Handlers = telemetry_handler_table:list_for_event(EventName),
    ApplyFun =
        fun(#handler{id=HandlerId,
                     function=HandlerFunction,
                     config=Config}) ->
            try
                HandlerFunction(EventName, EventValue, EventMetadata, Config)
            catch
                ?WITH_STACKTRACE(Class, Reason, Stacktrace)
                    detach(HandlerId),
                    ?LOG_ERROR("Handler ~p has failed and has been detached. "
                               "Exception: class=~p reason=~p stacktrace=~p",
                               [HandlerId, Class, Reason, Stacktrace])
            end
        end,

    lists:foreach(ApplyFun, Handlers).


%% @doc Returns all handlers attached to events with given prefix.
%%
%% Handlers attached to many events at once using `attach_many/4' will be listed once for each
%% event they're attached to.
%% Note that you can list all handlers by feeding this function an empty list.
-spec list_handlers(event_prefix()) -> [handler()].
list_handlers(EventPrefix) ->
    assert_event_name_or_prefix(EventPrefix),
    [#{id => HandlerId,
       event_name => EventName,
       function => Function,
       config => Config} || #handler{id=HandlerId,
                                     event_name=EventName,
                                     function=Function,
                                     config=Config} <- telemetry_handler_table:list_by_prefix(EventPrefix)].

%%

-spec assert_event_names_or_prefixes(term()) -> [ok].
assert_event_names_or_prefixes(List) when is_list(List) ->
    [assert_event_name_or_prefix(E) || E <- List].

-spec assert_event_name_or_prefix(term()) -> ok.
assert_event_name_or_prefix(List) when is_list(List) ->
    case lists:all(fun erlang:is_atom/1, List) of
        true ->
            ok;
        false ->
            erlang:error(badarg, List)
    end;
assert_event_name_or_prefix(List) ->
    erlang:error(badarg, List).
