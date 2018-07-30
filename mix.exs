defmodule Events.MixProject do
  use Mix.Project

  def project do
    [
      app: :events,
      version: "0.1.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      preferred_cli_env: ["bench.list_handlers_for_event": :bench],
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Events.Application, []}
    ]
  end

  defp elixirc_paths(:bench), do: ["lib/", "bench/"]
  defp elixirc_paths(_), do: ["lib/"]

  defp deps do
    [
      {:benchee, "~> 0.13", only: :bench},
      {:erlang_pmp, "~> 0.1"}
    ]
  end
end
