defmodule MapReduce.MixProject do
  use Mix.Project

  def project do
    [
      app: :map_reduce,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      dialyzer: [],
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:dialyxir, "~> 1.1", only: [:dev], runtime: false}
    ]
  end
end
