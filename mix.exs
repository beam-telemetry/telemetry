defmodule Telemetry.MixProject do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :telemetry,
      version: @version,
      elixir: "~> 1.4",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      preferred_cli_env: preferred_cli_env(),
      deps: deps(),
      docs: docs(),
      package: package()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Telemetry.Application, []}
    ]
  end

  defp elixirc_paths(:bench), do: ["lib/", "bench/"]
  defp elixirc_paths(_), do: ["lib/"]

  defp preferred_cli_env() do
    [
      docs: :docs,
      "bench.list_handlers_for_event": :bench,
      "bench.execute": :bench
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.19", only: :docs},
      {:benchee, "~> 0.13", only: :bench},
      {:erlang_pmp, "~> 0.1"}
    ]
  end

  defp docs do
    [
      main: "readme",
      name: "Telemetry",
      canonical: "http://hexdocs.pm/telemetry",
      source_url: "https://github.com/elixir-telemetry/telemetry",
      source_ref: "v#{@version}",
      extras: [
        "README.md"
      ]
    ]
  end

  defp package do
    [
      maintainers: ["Arkadiusz Gil", "José Valim"],
      licenses: ["Apache 2.0"],
      links: %{"GitHub" => "https://github.com/elixir-telemetry/telemetry"}
    ]
  end
end
