defmodule Events.Impl do
  @moduledoc false

  @callback child_spec(term()) :: Supervisor.child_spec()

  @callback subscribe(
              Events.subscription_id(),
              Events.event_prefix(),
              module,
              function :: atom,
              config :: map
            ) :: :ok | :error

  @callback unsubscribe(Events.subscription_id()) :: :ok | :error

  @callback list_subscribed_to(Events.event_name()) ::
              {Events.subscription_id(), Events.event_prefix(), module, function :: atom,
               config :: term}

  @callback list_subscriptions(Events.event_prefix()) ::
              {Events.subscription_id(), Events.event_prefix(), module, function :: atom,
               config :: term}
end
