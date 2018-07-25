defmodule Handler do
  def handle(_, _, _), do: :ok
end

{parsed, _} =
  OptionParser.parse!(
    System.argv(),
    switches: [parallelism: :integer],
    aliases: [p: :parallelism]
  )

parallelism = Keyword.get(parsed, :parallelism, System.schedulers_online())

segments = ?a..?z |> Enum.map(&List.wrap/1) |> Enum.map(&:erlang.list_to_atom/1)

impls = [Events.Impl.Agent, Events.Impl.Ets, Events.Impl.EtsPerf]

Supervisor.start_link(impls, strategy: :one_for_one)

for prefix_len <- 0..length(segments) do
  prefix = Enum.take(segments, prefix_len)

  for impl <- impls do
    :ok = impl.attach("id#{prefix_len}", prefix, Handler, :handle, nil)
  end
end

Benchee.run(
  %{
    "execute" => fn -> Events.execute(segments, 1) end
  },
  time: 10,
  memory_time: 2,
  parallel: parallelism
)
