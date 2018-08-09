defmodule Telemetry.Impl do
  @moduledoc false

  @callback child_spec(term()) :: Supervisor.child_spec()

  @callback attach(
              Telemetry.handler_id(),
              [Telemetry.event_name()],
              module,
              function :: atom,
              config :: map
            ) :: :ok | {:error, :already_exists}

  @callback detach(Telemetry.handler_id()) :: :ok | {:error, :not_found}

  @callback list_handlers_for_event(Telemetry.event_name()) ::
              {Telemetry.handler_id(), Telemetry.event_name(), module, function :: atom,
               config :: term}

  @callback list_handlers_by_prefix(Telemetry.event_name()) ::
              {Telemetry.handler_id(), Telemetry.event_name(), module, function :: atom,
               config :: term}
end
