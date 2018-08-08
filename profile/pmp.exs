Code.require_file("suite.ex", __DIR__)

{parsed, _} =
  OptionParser.parse!(System.argv(), switches: [workers: :integer], aliases: [w: :workers])

workers = Keyword.get(parsed, :workers, System.schedulers_online())

IO.puts(:stderr, "Profiling with #{workers} workers")

segments = ?a..?z |> Enum.map(&List.wrap/1) |> Enum.map(&:erlang.list_to_atom/1)
Suite.setup(segments, workers: workers)
pids = Suite.get_worker_pids()
Suite.run()
IO.puts("Warming up..")
Process.sleep(10_000)
IO.puts("Profiling..")

pid =
  :erlang_pmp.profile(
    duration: 10,
    filename: Path.join(__DIR__, "../pmp.trace") |> to_charlist(),
    processes: pids,
    sleep: 1,
    show_status: true
  )

ref = Process.monitor(pid)

receive do
  {:DOWN, ^ref, :process, ^pid, reason} ->
    IO.puts("Finished with #{inspect(reason)}")
end

IO.puts("Done")

Suite.teardown()
