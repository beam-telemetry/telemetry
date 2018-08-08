defmodule Telemetry.Impl.Agent do
  @moduledoc false
  # Dummy implementation, not really performant but good for designing the APIs.

  @behaviour Telemetry.Impl

  @impl true
  def child_spec(_) do
    %{
      id: __MODULE__,
      start: {Agent, :start_link, [fn -> %{} end, [name: __MODULE__]]}
    }
  end

  @impl true
  def attach(handler_id, event_names, module, function, config) do
    Agent.get_and_update(__MODULE__, fn handlers ->
      if Map.has_key?(handlers, handler_id) do
        {{:error, :already_exists}, handlers}
      else
        {:ok, Map.put(handlers, handler_id, {handler_id, event_names, module, function, config})}
      end
    end)
  end

  @impl true
  def detach(handler_id) do
    Agent.get_and_update(__MODULE__, fn handlers ->
      if Map.has_key?(handlers, handler_id) do
        {:ok, Map.delete(handlers, handler_id)}
      else
        {{:error, :not_found}, handlers}
      end
    end)
  end

  @impl true
  def list_handlers_for_event(event_name) do
    handlers = Agent.get(__MODULE__, fn handlers -> Map.values(handlers) end)

    Enum.flat_map(handlers, fn {handler_id, event_names, module, function, config} ->
      event_names
      |> Enum.filter(fn handlers_event -> handlers_event == event_name end)
      |> Enum.map(fn handlers_event -> {handler_id, handlers_event, module, function, config} end)
    end)
  end

  @impl true
  def list_handlers_by_prefix(prefix) do
    handlers = Agent.get(__MODULE__, fn handlers -> Map.values(handlers) end)

    Enum.flat_map(handlers, fn {handler_id, event_names, module, function, config} ->
      event_names
      |> Enum.filter(fn event_name -> event_prefix?(prefix, event_name) end)
      |> Enum.map(fn event_name -> {handler_id, event_name, module, function, config} end)
    end)
  end

  @spec event_prefix?(Telemetry.event_prefix(), Telemetry.event_name()) :: boolean()
  defp event_prefix?(prefix, event_name) do
    prefix_len = length(prefix)
    Enum.take(event_name, prefix_len) == prefix
  end
end
