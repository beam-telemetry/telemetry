defmodule Events.Impl do
  @moduledoc false

  @callback child_spec(term()) :: Supervisor.child_spec()

  @callback attach(
              Events.handler_id(),
              Events.event_prefix(),
              module,
              function :: atom,
              config :: map
            ) :: :ok | {:error, :already_exists}

  @callback detach(Events.handler_id()) :: :ok | {:error, :not_found}

  @callback list_attached_to(Events.event_name()) ::
              {Events.handler_id(), Events.event_prefix(), module, function :: atom,
               config :: term}

  @callback list_handlers(Events.event_prefix()) ::
              {Events.handler_id(), Events.event_prefix(), module, function :: atom,
               config :: term}
end
