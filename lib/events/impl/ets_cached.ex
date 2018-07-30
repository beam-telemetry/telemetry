defmodule Events.Impl.EtsCached do
  @moduledoc false
  # Implementation based on two ETS tables: regular table for handlers as in `Ets` implementation
  # and a cache table.
  #
  # If the cache is disabled (more on when it's enabled later) the reads works exactly like in
  # the `Ets` implementation.
  #
  # The cache is a set table with read concurrency. The key is an event name (not a prefix!) and
  # the values are lists of handler records as stored in regular handler table. As you might suspect,
  # the list contains all handlers which should be invoked when the event stored as key is emitted.
  #
  # When the cache is enabled, `list_handlers_for_event/1` first checks if the handlers which
  # should be invoked for the event are not already cached. If they are, then those handlers are
  # returned. If they are not, the handlers are looked up in regular table, exactly the same as
  # in `Ets` implementation, and then they are cached and returned from the function.
  #
  # In this implementation Agent has been replaced with the GenServer. Same as in `Ets` backend,
  # all writes go though this process. However in this case, the GenServer is also responsible for
  # enabling and disabling the cache. The cache is enabled when there have been no writes for 500
  # milliseconds. When the cache is enabled and the write occurs, the whole cache is cleared and
  # the countdown starts again. The downside of this approach is that the cache will be cleared
  # when handlers are detached due to their failure.
  # The improvement here would be to delete only these cache entries which would be affected by the
  # write (fortunately this quite easy to achive and doesn't require computing the prefixes).
  # With such solution, the cache would be always enabled. It would probably be beneficial to
  # detach failing handlers in bathes to lower the number of writes to the table.
  # There is also a downside of this solution: all writes to cache table go through the GenServer.
  # It might happen that many processes emit the same event at once and all try to save handlers
  # in cache which might flood the process with messages. On the other hand, if the processes would
  # insert entries into cache by themselves, we could run into all sorts of concurrency issues.
  #
  # One thing missing from this implementation is cache invalidation. I believe that the LFU (least
  # frequently used) scheme would be the most appropriate here.

  use GenServer

  @behaviour Events.Impl

  @compile {:inline, match_spec_segment: 1}

  @table __MODULE__
  @cache_table __MODULE__.Cache
  @cache_enable_timeout 500

  @impl Events.Impl
  def child_spec(_) do
    %{
      id: __MODULE__,
      start: {GenServer, :start_link, [__MODULE__, [], [name: __MODULE__]]}
    }
  end

  @impl Events.Impl
  def attach(handler_id, prefix, module, function, config) do
    GenServer.call(__MODULE__, {:attach, handler_id, prefix, module, function, config})
  end

  @impl Events.Impl
  def detach(handler_id) do
    GenServer.call(__MODULE__, {:detach, handler_id})
  end

  @impl Events.Impl
  def list_handlers_for_event(event_name) do
    if cache_enabled?() do
      case lookup_in_cache(event_name) do
        {:ok, handlers} ->
          handlers

        {:error, :not_found} ->
          handlers = search_in_table(event_name)
          save_in_cache(event_name, handlers)
          handlers
      end
    else
      search_in_table(event_name)
    end
  end

  @impl Events.Impl
  def list_handlers_by_prefix(event_prefix) do
    pattern = match_pattern_for_prefix(event_prefix)
    :ets.match_object(@table, pattern)
  end

  @impl GenServer
  def init([]) do
    create_tables()

    state =
      %{cache_enable_timer: nil, cache_enabled: false}
      |> enable_cache()

    {:ok, state}
  end

  @impl GenServer
  def handle_call({:attach, handler_id, prefix, module, function, config}, _, state) do
    {state, reply} =
      if handler_exists?(handler_id) do
        {state, {:error, :already_exists}}
      else
        state = disable_cache_and_start_timer(state)
        :ets.insert(@table, {handler_id, prefix, module, function, config})
        {state, :ok}
      end

    {:reply, reply, state}
  end

  def handle_call({:detach, handler_id}, _, state) do
    {state, reply} =
      if handler_exists?(handler_id) do
        state = disable_cache_and_start_timer(state)
        :ets.match_delete(@table, {handler_id, :_, :_, :_, :_})
        {state, :ok}
      else
        {state, {:error, :not_found}}
      end

    {:reply, reply, state}
  end

  @impl GenServer
  def handle_cast({:save_in_cache, event_name, handlers}, state) do
    :ets.insert(@cache_table, {{:cached, event_name}, handlers})
    {:noreply, state}
  end

  @impl GenServer
  def handle_info(:enable_cache, state) do
    state = enable_cache(state)
    {:noreply, state}
  end

  defp create_tables() do
    :ets.new(@table, [:bag, :protected, :named_table, keypos: 2, read_concurrency: true])
    :ets.new(@cache_table, [:set, :protected, :named_table, keypos: 1, read_concurrency: true])
  end

  defp enable_cache(state) do
    :ets.insert(@cache_table, {:enabled, true})
    %{state | cache_enabled: true}
  end

  defp cache_enabled?() do
    case :ets.lookup(@cache_table, :enabled) do
      [{:enabled, enabled?}] ->
        enabled?

      [] ->
        false
    end
  end

  defp save_in_cache(event_name, handlers) do
    GenServer.cast(__MODULE__, {:save_in_cache, event_name, handlers})
  end

  defp search_in_table(event_name) do
    match_spec = match_spec_for_event(event_name)
    :ets.select(@table, match_spec)
  end

  defp lookup_in_cache(event_name) do
    case :ets.lookup(@cache_table, {:cached, event_name}) do
      [{{:cached, ^event_name}, handlers}] ->
        {:ok, handlers}

      [] ->
        {:error, :not_found}
    end
  end

  defp disable_cache_and_start_timer(%{cache_enabled: false} = state) do
    Process.cancel_timer(state.cache_enable_timer)
    timer_ref = Process.send_after(self(), :enable_cache, @cache_enable_timeout)
    %{state | cache_enable_timer: timer_ref}
  end

  defp disable_cache_and_start_timer(%{cache_enabled: true} = state) do
    :ets.delete_all_objects(@cache_table)
    :ets.insert(@cache_table, {:enabled, false})
    timer_ref = Process.send_after(self(), :enable_cache, @cache_enable_timeout)
    %{state | cache_enable_timer: timer_ref}
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

  defp match_spec_for_event([], prev_rev_prefix, acc) do
    [match_spec_segment(:lists.reverse(prev_rev_prefix)) | acc]
  end

  defp match_spec_for_event([segment | rest], prev_rev_prefix, acc) do
    match_spec_for_event(rest, [segment | prev_rev_prefix], [
      match_spec_segment(:lists.reverse(prev_rev_prefix)) | acc
    ])
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
