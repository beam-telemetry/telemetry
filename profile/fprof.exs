Code.require_file("suite.ex", __DIR__)

defmodule Runner do
  def run(workers) do
    segments = ?a..?z |> Enum.map(&List.wrap/1) |> Enum.map(&:erlang.list_to_atom/1)
    Suite.setup(segments, workers: workers)
    Suite.run()
    Process.sleep(10_000)
    Suite.teardown()
  end
end

{parsed, _} =
  OptionParser.parse!(System.argv(), switches: [workers: :integer], aliases: [w: :workers])

workers = Keyword.get(parsed, :workers, System.schedulers_online())

IO.puts(:stderr, "Profiling with #{workers} workers")

Mix.Tasks.Profile.Fprof.run(["-e", "Runner.run(#{workers})", "--callers"])
