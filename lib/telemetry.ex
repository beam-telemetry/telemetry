defmodule Telemetry do
  @moduledoc """
  `Telemetry` allows you to invoke certain functions whenever a particular event is emitted.

  For more information see the documentation for `attach/5`, `attach_many/5` and `execute/3`.
  """

  require Logger

  alias Telemetry.HandlerTable

  @type handler_id :: term()
  @type event_name :: [atom()]
  @type event_value :: number()
  @type event_metadata :: map()
  @type event_prefix :: [atom()]

  ## API

  @doc """
  Attaches the handler to the event.

  `handler_id` must be unique, if another handler with the same ID already exists the
  `{:error, :already_exists}` tuple is returned.

  See `execute/3` to learn how the handlers are invoked.
  """
  @spec attach(handler_id, event_name, module, function :: atom, config :: term) ::
          :ok | {:error, :already_exists}
  def attach(handler_id, event_name, module, function, config) do
    attach_many(handler_id, [event_name], module, function, config)
  end

  @doc """
  Attaches the handler to many events.

  The handler will be invoked whenever any of the events in the `event_names` list is emitted. Note
  that failure of the handler on any of these invokations will detach it from all the events in
  `event_name` (the same applies to manual detaching using `detach/1`).
  """
  @spec attach_many(handler_id, [event_name], module, function :: atom, config :: term) ::
          :ok | {:error, :already_exists}
  def attach_many(handler_id, event_names, module, function, config) do
    Enum.each(event_names, &assert_event_name_or_prefix/1)
    HandlerTable.insert(handler_id, event_names, module, function, config)
  end

  @doc """
  Removes the existing handler.

  If the handler with given ID doesn't exist, `{:error, :not_found}` is returned.
  """
  @spec detach(handler_id) :: :ok | {:error, :not_found}
  def detach(handler_id) do
    HandlerTable.delete(handler_id)
  end

  @doc """
  Emits the event, invoking handlers attached to it.

  When the event is emitted, `module.function` provided to `attach/5` is called with four arguments:
  * the event name
  * the event value
  * the event metadata
  * the handler configuration given to `attach/5`

  All the handlers are executed by the process calling this function. If the function fails (raises,
  exits or throws) then the handler is removed.

  Note that you should not rely on the order in which handlers are invoked.
  """
  @spec execute(event_name, event_value) :: :ok
  @spec execute(event_name, event_value, event_metadata) :: :ok
  def execute(event_name, value, metadata \\ %{})
      when is_number(value) and is_map(metadata) do
    handlers = HandlerTable.list_for_event(event_name)

    for {handler_id, _, module, function, config} <- handlers do
      try do
        apply(module, function, [event_name, value, metadata, config])
      catch
        class, reason ->
          detach(handler_id)

          stacktrace = System.stacktrace()

          Logger.error(
            "Handler #{inspect(module)}.#{function} with ID #{inspect(handler_id)} " <>
              "has failed and has been detached\n" <> Exception.format(class, reason, stacktrace)
          )
      end
    end

    :ok
  end

  @doc """
  Returns all handlers attached to events with given prefix.

  Handlers attached to many events at once using `attach_many/5` will be listed once for each
  event they're attached to.

  Note that you can list all handlers by feeding this function an empty list.
  """
  @spec list_handlers(event_prefix) :: [
          {handler_id, event_name, module, function :: atom, config :: term}
        ]
  def list_handlers(event_prefix) do
    assert_event_name_or_prefix(event_prefix)

    HandlerTable.list_by_prefix(event_prefix)
  end

  ## Helpers

  @spec assert_event_name_or_prefix(term()) :: :ok | no_return
  defp assert_event_name_or_prefix(list) when is_list(list) do
    if Enum.all?(list, &is_atom/1) do
      :ok
    else
      raise ArgumentError, "Expected event name or prefix to be a list of atoms"
    end
  end

  defp assert_event_name_or_prefix(_) do
    raise ArgumentError, "Expected event name or prefix to be a list of atoms"
  end
end
