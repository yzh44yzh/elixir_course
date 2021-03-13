defmodule HelloWorld.MixProject do
  use Mix.Project

  def project do
    [
      app: :lesson_06,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: [
        paths: ["ebin"]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: []
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:dialyxir, "1.0.0-rc.6", only: [:dev], runtime: false}
    ]
  end
end
