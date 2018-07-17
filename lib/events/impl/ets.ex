defmodule Events.Impl.Ets do
  @moduledoc false
  # Implementation based on a single ETS table
  # All writes go through an Agent to guarantee uniqueness of subscription IDs.  Reads are executed directly on the ETS table by the calling process. Processes do not lock the
  # table, so it might happend that other process attaches a handler while the process iterates
  # through the table to find matching handlers.

  @behaviour Events.Impl

  @table __MODULE__

  @impl true
  def child_spec(_) do
    %{
      id: __MODULE__,
      start: {Agent, :start_link, [&create_table/0, [name: __MODULE__]]}
    }
  end

  @impl true
  def attach(handler_id, prefix, module, function, config) do
    Agent.get_and_update(__MODULE__, fn table ->
      if handler_exists?(handler_id) do
        {{:error, :already_exists}, table}
      else
        :ets.insert(table, {handler_id, prefix, module, function, config})
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
    patterns = match_patterns_for_event(event_name)

    match_spec =
      for pattern <- patterns do
        {pattern, [], [:"$_"]}
      end

    :ets.select(@table, match_spec)
  end

  @impl true
  def list_handlers_by_prefix(event_prefix) do
    pattern = match_pattern_for_prefix(event_prefix)
    :ets.match_object(@table, pattern)
  end

  defp create_table() do
    :ets.new(@table, [:bag, :protected, :named_table, keypos: 2, read_concurrency: true])
  end

  @spec handler_exists?(Events.handler_id()) :: boolean()
  defp handler_exists?(handler_id) do
    case :ets.match(@table, {handler_id, :_, :_, :_, :_}) do
      [_] ->
        true

      [] ->
        false
    end
  end

  @spec match_patterns_for_event(Events.event_name()) :: [:ets.match_pattern()]
  defp match_patterns_for_event(event_name) do
    for prefix <- generate_prefixes(event_name) do
      {:_, prefix, :_, :_, :_}
    end
  end

  @spec generate_prefixes(Events.event_name()) :: [Events.event_prefix()]
  defp generate_prefixes([]), do: [[]]
  defp generate_prefixes([_] = event_name), do: [[], event_name]

  defp generate_prefixes(event_name) do
    prefixes =
      for prefix_len <- 1..(length(event_name) - 1) do
        Enum.take(event_name, prefix_len)
      end

    [[], event_name | prefixes]
  end

  @spec match_pattern_for_prefix(Events.event_prefix()) :: :ets.match_pattern()
  defp match_pattern_for_prefix(event_prefix) do
    {:_, match_for_prefix(event_prefix), :_, :_, :_}
  end

  defp match_for_prefix([]), do: :_

  defp match_for_prefix([segment | tail]) do
    [segment | match_for_prefix(tail)]
  end
end
