defmodule Handler do
  def handle(event, _, _) do
    event
  end
end

defmodule Worker do
  def init(segments) do
    segments_count = length(segments)

    receive do
      :go ->
        loop(segments, segments_count)
    end
  end

  defp loop(segments, segments_count) do
    # this way all handlers will be executed - worst case scenario
    Events.execute(segments, 0)
    loop(segments, segments_count)
  end
end

defmodule Coordinator do
  use GenServer

  def start_link(segments, opts) do
    GenServer.start_link(__MODULE__, [segments, opts], name: __MODULE__)
  end

  def get_worker_pids() do
    GenServer.call(__MODULE__, :get_worker_pids)
  end

  def run() do
    GenServer.cast(__MODULE__, :run)
  end

  def stop() do
    GenServer.cast(__MODULE__, :stop)
  end

  @impl true
  def init([segments, opts]) do
    Process.flag(:trap_exit, true)
    workers = Keyword.get(opts, :workers, System.schedulers_online())

    worker_pids =
      for _ <- 1..workers do
        spawn_link(fn -> Worker.init(segments) end)
      end

    {:ok, %{worker_pids: worker_pids}}
  end

  @impl true
  def handle_call(:get_worker_pids, _, state) do
    {:reply, state.worker_pids, state}
  end

  @impl true
  def handle_cast(:run, state) do
    for pid <- state.worker_pids do
      send(pid, :go)
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast(:stop, state) do
    for pid <- state.worker_pids do
      Process.exit(pid, :kill)
    end

    {:stop, :normal, state}
  end

  @impl true
  def handle_info({:EXIT, pid, reason}, state) do
    if pid in state.worker_pids do
      {:stop, {:worker_down, pid, reason}, state}
    else
      {:noreply, state}
    end
  end
end

defmodule Suite do
  def setup(segments, opts \\ []) do
    for prefix_len <- 0..length(segments) do
      prefix = Enum.take(segments, prefix_len)
      :ok = Events.attach("id#{prefix_len}", prefix, Handler, :handle)
    end

    Coordinator.start_link(segments, opts)
  end

  def run() do
    Coordinator.run()
  end

  def get_worker_pids() do
    Coordinator.get_worker_pids()
  end

  def teardown() do
    for {id, _, _, _, _} <- Events.list_handlers([]), do: :ok = Events.detach(id)
    Coordinator.stop()
  end
end
