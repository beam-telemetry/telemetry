defmodule Telemetry.Impl.Ets do
  @moduledoc false
  # Implementation based on a single ETS bag table with read concurrency.
  #
  # Each handler is stored in a table. A key is an event name the handler is attached to. All writes
  # to a table go through a single Agent process to make sure that we don't get duplicate handler IDs.
  #
  # Reads (`list_handlers_...`) are executed by the calling process.

  @behaviour Telemetry.Impl

  @table __MODULE__
  @impl true
  def child_spec(_) do
    %{
      id: __MODULE__,
      start: {Agent, :start_link, [&create_table/0, [name: __MODULE__]]}
    }
  end

  @impl true
  def attach(handler_id, event_names, module, function, config) do
    Agent.get_and_update(__MODULE__, fn table ->
      if handler_exists?(handler_id) do
        {{:error, :already_exists}, table}
      else
        objects =
          Enum.map(event_names, fn event_name ->
            {handler_id, event_name, module, function, config}
          end)

        :ets.insert(table, objects)
        {:ok, table}
      end
    end)
  end

  @impl true
  def detach(handler_id) do
    Agent.get_and_update(__MODULE__, fn table ->
      if handler_exists?(handler_id) do
        :ets.match_delete(table, {handler_id, :_, :_, :_, :_})
        {:ok, table}
      else
        {{:error, :not_found}, table}
      end
    end)
  end

  @impl true
  def list_handlers_for_event(event_name) do
    :ets.lookup(@table, event_name)
  end

  @impl true
  def list_handlers_by_prefix(event_prefix) do
    pattern = match_pattern_for_prefix(event_prefix)
    :ets.match_object(@table, pattern)
  end

  defp create_table() do
    :ets.new(@table, [:duplicate_bag, :protected, :named_table, keypos: 2, read_concurrency: true])
  end

  @spec handler_exists?(Telemetry.handler_id()) :: boolean()
  defp handler_exists?(handler_id) do
    case :ets.match(@table, {handler_id, :_, :_, :_, :_}) do
      [_ | _] ->
        true

      [] ->
        false
    end
  end

  @spec match_pattern_for_prefix(Telemetry.event_prefix()) :: :ets.match_pattern()
  defp match_pattern_for_prefix(event_prefix) do
    {:_, match_for_prefix(event_prefix), :_, :_, :_}
  end

  defp match_for_prefix([]), do: :_

  defp match_for_prefix([segment | tail]) do
    [segment | match_for_prefix(tail)]
  end
end
