defmodule Mix.Tasks.Bench.ListHandlersForEvent do
  @moduledoc """
  Runs a benchmark of `list_handlers_for_event/1` callback of various implementations.

  The benchmark spawns processes executing `Events.Impl.list_events_for_event/1` callback in a loop
  using `Benchee`. The number of spawned processes can be configured using the `--parallelism`
  option. You can also specify how many handlers will be attached using the `--handlers-count`
  option. The event invoked during the benchmark is selected so that all attached handlers are be the
  result of the `list_handlers_for_event/1` call.

  ## Command line options

  * `--parallelism`, `-p` - how many simultaneous processes will be executing the function, defaults
    to number of core the benchmark is running on
  * `--handlers-count`, `-h` - how many handlers will be attached, defaults to 100
  * `--duration`, `-d` - how long the benchmark will run (in seconds), defaults to 10 seconds
  * `--save` - path to a file where the results of the benchmark will be saved. The same path may
    be provided in subsequent runs with the `--load` option to compare the new results with the
    saved ones
  * `--load` - path to a file where the results will be loaded from
  * `--save-tag` - "name" for the benchmark results helping to differentiate multiple runs when
    using the `--load` option. Does not have any effect unless  `--save` is provided as well
  """

  use Mix.Task

  @shortdoc "Runs a benchmark of `list_handlers_for_event/1` callback of various implementations"

  @switches [
    parallelism: :integer,
    duration: :integer,
    handlers_count: :integer,
    save: :string,
    save_tag: :string,
    load: :string
  ]
  @aliases [p: :parallelism, d: :duration, h: :handlers_count]

  @impls %{
    "Agent" => Events.Impl.Agent,
    "ETS" => Events.Impl.Ets,
    "ETS with cache" => Events.Impl.EtsCached,
    "ETS with lookup" => Events.Impl.EtsLookup
  }

  def run(argv) do
    {opts, _, _} = OptionParser.parse(argv, switches: @switches, aliases: @aliases)

    parallelism = Keyword.get(opts, :parallelism, System.schedulers_online())
    handlers_count = opts |> Keyword.get(:handlers_count, 100) |> normalize_handlers_count()
    duration = Keyword.get(opts, :duration, 10)

    event = setup(handlers_count)

    benchee_opts =
      [parallel: parallelism, time: duration] ++ maybe_save_opts(opts) ++ maybe_load_opts(opts)

    Benchee.run(benchmark_spec(event), benchee_opts)
  end

  defp setup(handlers_count) do
    Mix.shell().info("Setting up benchmark...")
    impl_modules = Map.values(@impls)

    Mix.shell().info("Starting implementations...")
    {:ok, _pid} = Supervisor.start_link(impl_modules, strategy: :one_for_one, max_restarts: 0)

    Mix.shell().info("Attaching #{handlers_count} handlers...")

    for num <- handler_numbers(handlers_count) do
      for impl <- impl_modules do
        :ok = impl.attach(num, handler_prefix(num), Handler, :handle, nil)
      end
    end

    covering_event_name(handlers_count)
  end

  defp normalize_handlers_count(count) when count < 0 do
    Mix.shell().info(
      "Requested handlers count is less than 0 (#{count}). Falling back to default 100."
    )

    100
  end

  defp normalize_handlers_count(count) do
    count
  end

  defp handler_numbers(0) do
    []
  end

  defp handler_numbers(count) do
    1..count
  end

  defp handler_prefix(num) do
    Enum.map(1..num, &(&1 |> to_string() |> :erlang.binary_to_atom(:latin1)))
  end

  defp covering_event_name(0), do: []
  defp covering_event_name(count), do: handler_prefix(count)

  defp benchmark_spec(event) do
    for {name, impl_module} <- @impls, into: %{} do
      {name, fn -> impl_module.list_handlers_for_event(event) end}
    end
  end

  defp maybe_save_opts(opts) do
    case Keyword.fetch(opts, :save) do
      {:ok, save_path} ->
        current_ts =
          :calendar.local_time() |> NaiveDateTime.from_erl!() |> NaiveDateTime.to_iso8601()

        save_tag = Keyword.get(opts, :save_tag, current_ts)
        [save: [path: save_path, tag: save_tag]]

      :error ->
        []
    end
  end

  defp maybe_load_opts(opts) do
    case Keyword.fetch(opts, :load) do
      {:ok, load_path} ->
        [load: load_path]

      :error ->
        []
    end
  end
end
