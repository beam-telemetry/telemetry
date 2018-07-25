defmodule Events.MixProject do
  use Mix.Project

  def project do
    [
      app: :events,
      version: "0.1.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Events.Application, []}
    ]
  end

  defp deps do
    [
      {:benchee, "~> 0.13"},
      {:erlang_pmp, "~> 0.1"}
    ]
  end
end
