defmodule MyMusicBand.MixProject do
  use Mix.Project

  def project do
    [
      app: :my_music_band,
      version: "0.1.0",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
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
      {:dialyxir, "~> 1.4", only: [:dev], runtime: false}
    ]
  end
end
