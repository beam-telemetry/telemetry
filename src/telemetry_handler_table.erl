%%%-------------------------------------------------------------------
%% @private ETS table for handlers.
%%
%% Each handler is stored in the table. A key is an event name the
%% handler is attached to. All writes to a table go through a single
%% Agent process to make sure that we don't get duplicate handler IDs.
%%
%% Reads (`list_handlers_...') are executed by the calling process.
%% @end
%%%-------------------------------------------------------------------
-module(telemetry_handler_table).

-behaviour(gen_server).

-export([start_link/0,
         insert/4,
         delete/1,
         list_for_event/1,
         list_by_prefix/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include("telemetry.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec insert(HandlerId, EventNames, Function, Config) -> ok | {error, already_exists} when
      HandlerId :: telemetry:handler_id(),
      EventNames :: [telemetry:event_name()],
      Function :: telemetry:handler_function(),
      Config :: telemetry:handler_config().
insert(HandlerId, EventNames, Function, Config) ->
    gen_server:call(?MODULE, {insert, HandlerId, EventNames, Function, Config}).

-spec delete(telemetry:handler_id()) -> ok | {error, not_found}.
delete(HandlerId) ->
    gen_server:call(?MODULE, {delete, HandlerId}).

impl_get() -> persistent_term:get(telemetry).

-spec list_for_event(telemetry:event_name()) -> [#handler{}].
list_for_event(EventName) ->
    case impl_get() of
        {ets, TID} ->
            try
                ets:lookup(TID, EventName)
            catch
                error:badarg ->
                    persistent_term:erase(telemetry),
                    ?LOG_WARNING("Failed to lookup telemetry handlers. "
                                 "Ensure the telemetry application has been started. ", []),
                    []
            end;
        _ ->
            ?LOG_WARNING("Failed to lookup telemetry handlers. "
                         "Ensure the telemetry application has been started. ", []),
            []
    end.

-spec list_by_prefix(telemetry:event_prefix()) -> [#handler{}].
list_by_prefix(EventPrefix) ->
    case impl_get() of
        {ets, TID} ->
            Pattern = match_pattern_for_prefix(EventPrefix),
            ets:match_object(TID, Pattern);
        _ ->
            ?LOG_WARNING("Failed to lookup telemetry handlers. "
                         "Ensure the telemetry application has been started. ", []),
            []
    end.

init([]) ->
    TID = create_table(),

    persistent_term:put(telemetry, {ets, TID}),

    {ok, []}.

handle_call({insert, HandlerId, EventNames, Function, Config}, _From, State) ->
    case impl_get() of
        {ets, TID} ->
            case ets:match(TID, #handler{id=HandlerId,
                                         _='_'}) of
                [] ->
                    Objects = [#handler{id=HandlerId,
                                        event_name=EventName,
                                        function=Function,
                                        config=Config} || EventName <- EventNames],
                    ets:insert(TID, Objects),
                    {reply, ok, State};
                _ ->
                    {reply, {error, already_exists}, State}
            end
    end;
handle_call({delete, HandlerId}, _From, State) ->
    case impl_get() of
        {ets, TID} ->
            case ets:select_delete(TID, [{#handler{id=HandlerId,
                                                   _='_'}, [], [true]}]) of
                0 ->
                    {reply, {error, not_found}, State};
                _ ->
                    {reply, ok, State}
            end
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_Reason, _State) ->
    persistent_term:erase(telemetry),
    ok.

%%

create_table() ->
    ets:new(?MODULE, [duplicate_bag, protected, named_table,
                      {keypos, #handler.event_name}, {read_concurrency, true}]).

match_pattern_for_prefix(EventPrefix) ->
    #handler{event_name=match_for_prefix(EventPrefix),
             _='_'}.

-dialyzer({nowarn_function, match_for_prefix/1}).
match_for_prefix([]) ->
    '_';
match_for_prefix([Segment | Rest]) ->
    [Segment | match_for_prefix(Rest)].
