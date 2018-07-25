defmodule Events.Impl.EtsPerf do
  @moduledoc false
  # Implementation based on a single ETS table
  # All writes go through an Agent to guarantee uniqueness of subscription IDs.  Reads are executed directly on the ETS table by the calling process. Processes do not lock the
  # table, so it might happend that other process attaches a handler while the process iterates
  # through the table to find matching handlers.

  @behaviour Events.Impl

  @compile {:inline, match_spec_segment: 1}

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
    match_spec = match_spec_for_event(event_name)
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

  @spec match_spec_for_event(Events.event_name(), Events.event_prefix(), [tuple()]) ::
          :ets.match_spec()
  defp match_spec_for_event(event, last_prefix \\ [], acc \\ [])

  defp match_spec_for_event([], last_prefix, acc) do
    [match_spec_segment(last_prefix) | acc]
  end

  defp match_spec_for_event([segment | rest], last_prefix, acc) do
    match_spec_for_event(rest, last_prefix ++ [segment], [match_spec_segment(last_prefix) | acc])
  end

  @spec match_spec_segment(Events.event_prefix()) :: tuple()
  defp match_spec_segment(prefix) do
    {{:_, prefix, :_, :_, :_}, [], [:"$_"]}
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
