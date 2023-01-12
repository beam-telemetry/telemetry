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

-export([attach_event_handlers/2, handle_event/4]).

%% @doc Attaches a "message" handler to the given events.
%%
%% The attached handler sends a message to `destination_pid' every time it handles one of the
%% events in `events'. The function returns a reference that you can use to make sure that
%% messages come from this handler. This reference is also used as the handler ID, so you
%% can use it to detach the handler with {@link telemetry:detach/1}.
%%
%% The shape of messages sent to `destination_pid' is:
%%
%% ```
%% {Event, Ref, Measurements, Metadata}
%% '''
%%
%% For example, in Erlang a test could look like this:
%%
%% ```
%% Ref = telemetry_test:attach_event_handlers(self(), [[some, event]]),
%% function_that_emits_the_event(),
%% receive
%%     {[some, event], Ref, #{measurement := _}, #{meta := _}} ->
%%         telemetry:detach(Ref)
%% after 1000 ->
%%     ct:fail(timeout_receive_attach_event_handlers)
%% end.
%% '''
%%
%% In Elixir, a similar test would look like this:
%%
%% ```
%% ref = :telemetry_test.attach_event_handlers(self(), [[:some, :event]])
%% function_that_emits_the_event()
%% assert_received {[:some, :event], ^ref, %{measurement: _}, %{meta: _}}
%% '''
%%
-spec attach_event_handlers(DestinationPID, Events) -> reference() when
    DestinationPID :: pid(),
    Events :: [telemetry:event_name(), ...].
attach_event_handlers(DestPID, Events) when is_pid(DestPID) and is_list(Events) ->
    Ref = make_ref(),
    Config = #{dest_pid => DestPID, ref => Ref},
    telemetry:attach_many(Ref, Events, fun telemetry_test:handle_event/4, Config),
    Ref.

handle_event(Event, Measurements, Metadata, #{dest_pid := DestPID, ref := Ref}) ->
    DestPID ! {Event, Ref, Measurements, Metadata}.
