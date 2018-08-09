defmodule Telemetry.Impl do
  @moduledoc false

  @callback start_link() ::
              {:ok, pid()}
              | :ignore
              | {:error, {:already_started, pid()} | term()}

  @callback attach(
              Telemetry.handler_id(),
              Telemetry.event_prefix(),
              module,
              function :: atom,
              config :: map
            ) :: :ok | {:error, :already_exists}

  @callback detach(Telemetry.handler_id()) :: :ok | {:error, :not_found}

  @callback list_handlers_for_event(Telemetry.event_name()) ::
              {Telemetry.handler_id(), Telemetry.event_prefix(), module, function :: atom,
               config :: term}

  @callback list_handlers_by_prefix(Telemetry.event_prefix()) ::
              {Telemetry.handler_id(), Telemetry.event_prefix(), module, function :: atom,
               config :: term}
end
