defmodule Events do
  @moduledoc """
  Events allows to subscribe functions to events, and call these functions when the events are
  emitted.

  Note that all subscribed functions are called in the process which emits the event.
  """

  @callback_mod Application.fetch_env!(:events, :impl)

  @type subscription_id :: term()
  @type event_name :: list()
  @type event_prefix :: list()

  @doc """
  Creates a subscription to an event.

  `subscription_id` must be unique, if another subscription with the same ID already exists the
  `{:error, :already_exists}` atom is be returned.

  When events with `event_prefix` are emitted, function `function` in module `module` will be
  called with three arguments:
  * the exact event name (see `emit/2`)
  * the event value (see `emit/2`)
  * the subscription configuration, `config`, or `nil` if one wasn't provided

  If the function fails (raises, exits or throws) then the subscription is removed.
  """
  @spec subscribe(subscription_id, event_prefix, module, function :: atom) ::
          :ok | {:error, :already_exists}
  @spec subscribe(subscription_id, event_prefix, module, function :: atom, config :: term) ::
          :ok | {:error, :already_exists}
  defdelegate subscribe(sub_id, event_prefix, module, function, config \\ nil), to: @callback_mod

  @doc """
  Removes existing subscription.

  If the subscription doesn't exist, `{:error, :not_found}` is returned.
  """
  @spec unsubscribe(subscription_id) :: :ok | {:error, :not_found}
  defdelegate unsubscribe(sub_id), to: @callback_mod

  @doc """
  Emits an event with given name and value.

  All functions subscribed to the prefix of the `event_name` will be called. Note that you should
  not rely on the order in which those functions are called.
  """
  @spec emit(event_name, value :: term()) :: :ok
  def emit(event_name, value) do
    subscribed = @callback_mod.list_subscribed_to(event_name)

    for {sub_id, _, module, function, config} <- subscribed do
      try do
        apply(module, function, [event_name, value, config])
      catch
        _, _ ->
          # it might happen that other processes will call it before it's unsubscribed
          unsubscribe(sub_id)
      end
    end
  end

  @doc """
  Returns all subscriptions to events with given prefix.

  Note that you can list all subscriptions by feeding this function an empty list.
  """
  @spec list_subscriptions(event_prefix) ::
          {subscription_id, event_prefix, module, function :: atom, config :: map}
  defdelegate list_subscriptions(event_prefix), to: @callback_mod

  @doc false
  defdelegate child_spec(term), to: @callback_mod
end
