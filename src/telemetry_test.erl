%%%-------------------------------------------------------------------
%% @doc Functions for testing execution of Telemetry events.
%%
%% Testing that the correct Telemetry events are emitted with the
%% right measurements and metadata is essential for library authors.
%% It helps to maintain stable APIs and avoid accidental changes
%% to events.
%% @end
%%%-------------------------------------------------------------------

-module(telemetry_test).

-export([attach_event_handlers/2]).

%% @doc Attaches a "message" handler to the given events.
%%
%% `handler_id` must be unique and is used as the handler ID.
%%
%% The attached handler sends a message to `destination_pid` every time it handles one of the
%% events in `events`. The function returns a reference that you can use to make sure that
%% messages come from this handler. The shape of messages sent to `destination_pid` is:
%%
%% ```
%% {Event, Ref, Measurements, Metadata}
%% '''
%%
%% You can detach this event with {link telemetry:detach/1}.
-spec attach_event_handlers(DestinationPID, Events) -> {telemetry:handler_id(), reference()} when
    DestinationPID :: pid(),
    Events :: [telemetry:event_name(), ...].
attach_event_handlers(DestPID, Events) when is_pid(DestPID) and is_list(Events) ->
    Ref = make_ref(),
    HandlerId = crypto:strong_rand_bytes(16),
    Config = #{dest_pid => DestPID, ref => Ref},
    telemetry:attach_many(HandlerId, Events, fun handle_event/4, Config),
    {HandlerId, Ref}.

handle_event(Event, Measurements, Metadata, #{dest_pid := DestPID, ref := Ref}) ->
    DestPID ! {Event, Ref, Measurements, Metadata}.
