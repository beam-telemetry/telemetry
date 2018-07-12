defmodule Events.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      Events.Impl
    ]

    opts = [strategy: :one_for_one, name: Events.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
