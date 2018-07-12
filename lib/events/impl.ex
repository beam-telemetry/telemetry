defmodule Events.Impl do
  @moduledoc false

  defmodule Callbacks do
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

  @callback_mod Application.fetch_env!(:events, :impl)

  defdelegate child_spec(term), to: @callback_mod

  defdelegate subscribe(sub_id, prefix, module, function, config), to: @callback_mod

  defdelegate unsubscribe(sub_id), to: @callback_mod

  defdelegate list_subscribed_to(event), to: @callback_mod

  defdelegate list_subscriptions(prefix), to: @callback_mod
end
