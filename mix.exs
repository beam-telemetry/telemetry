defmodule Telemetry.MixProject do
  use Mix.Project

  @version "0.2.0"

  def project do
    [
      app: :telemetry,
      name: "Telemetry",
      version: @version,
      elixir: "~> 1.4",
      start_permanent: Mix.env() == :prod,
      preferred_cli_env: preferred_cli_env(),
      deps: deps(),
      docs: docs(),
      description: description(),
      package: package()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Telemetry.Application, []}
    ]
  end

  defp preferred_cli_env() do
    [
      docs: :docs,
      dialyzer: :test
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.19", only: :docs},
      {:erlang_pmp, "~> 0.1", only: :profile},
      {:dialyxir, "~> 1.0.0-rc.1", only: :test}
    ]
  end

  defp docs do
    [
      main: "readme",
      canonical: "http://hexdocs.pm/telemetry",
      source_url: "https://github.com/elixir-telemetry/telemetry",
      source_ref: "v#{@version}",
      extras: [
        "README.md"
      ]
    ]
  end

  defp description do
    """
    Dynamic dispatching library for metrics and instrumentations.
    """
  end

  defp package do
    [
      maintainers: ["Arkadiusz Gil", "José Valim"],
      licenses: ["Apache 2.0"],
      links: %{"GitHub" => "https://github.com/elixir-telemetry/telemetry"}
    ]
  end
end
