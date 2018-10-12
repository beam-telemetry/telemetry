defmodule Telemetry.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      worker(Telemetry.HandlerTable, [])
    ]

    opts = [strategy: :one_for_one, name: Telemetry.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
