defmodule BeamInspect.MixProject do
  use Mix.Project

  def project do
    [
      app: :beam_inspect,
      version: "0.1.0",
      elixir: "~> 1.3",
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
      {:ex_doc, "~> 0.13", only: :dev, runtime: false}
    ]
  end
end
