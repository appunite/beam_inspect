defmodule BeamInspect.MixProject do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :beam_inspect,
      version: @version,
      elixir: "~> 1.3",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package()
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

  defp elixirc_paths(:dev), do: ["lib", "test/support"]
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp package do
    [
      name: :beam_inspect,
      maintainers: ["Tobiasz Małecki"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/appunite/beam_inspect"
      }
    ]
  end
end
