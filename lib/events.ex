defmodule Events do
  @moduledoc """
  Events allows to attach functions to be called when events are emitted

  Note that all subscribed functions are called in the process which emits the event.
  """

  @callback_mod Application.fetch_env!(:events, :impl)

  @type handler_id :: term()
  @type event_name :: list()
  @type event_prefix :: list()

  @doc """
  Attaches a handler to the event prefix.

  `handler_id` must be unique, if another handler with the same ID already exists the
  `{:error, :already_exists}` tuple is returned.

  When events with `event_prefix` are emitted, function `function` in module `module` will be
  called with three arguments:
  * the exact event name (see `execute/2`)
  * the event value (see `execute/2`)
  * the handler configuration, `config`, or `nil` if one wasn't provided

  If the function fails (raises, exits or throws) then the handler is removed.
  """
  @spec attach(handler_id, event_prefix, module, function :: atom) ::
          :ok | {:error, :already_exists}
  @spec attach(handler_id, event_prefix, module, function :: atom, config :: term) ::
          :ok | {:error, :already_exists}
  defdelegate attach(handler_id, event_prefix, module, function, config \\ nil), to: @callback_mod

  @doc """
  Removes existing handler.

  If the handler with given ID doesn't exist, `{:error, :not_found}` is returned.
  """
  @spec detach(handler_id) :: :ok | {:error, :not_found}
  defdelegate detach(handler_id), to: @callback_mod

  @doc """
  Emits an event, executing handlers attached to all of its prefixes.

  Note that you should not rely on the order in which those functions are called.
  """
  @spec execute(event_name, value :: term()) :: :ok
  def execute(event_name, value) do
    handlers = @callback_mod.list_attached_to(event_name)

    for {handler_id, _, module, function, config} <- handlers do
      try do
        apply(module, function, [event_name, value, config])
      catch
        _, _ ->
          # it might happen that other processes will call it before it's detached
          detach(handler_id)
      end
    end
  end

  @doc """
  Returns all handlers attached to events with given prefix.

  Note that you can list all handlers by feeding this function an empty list.
  """
  @spec list_handlers(event_prefix) ::
          {handler_id, event_prefix, module, function :: atom, config :: map}
  defdelegate list_handlers(event_prefix), to: @callback_mod

  @doc false
  defdelegate child_spec(term), to: @callback_mod
end
