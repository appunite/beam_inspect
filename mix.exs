defmodule BeamInspect.MixProject do
  use Mix.Project

  @version "0.1.1"

  def project do
    [
      app: :beam_inspect,
      deps: deps(),
      description: "Inspect how your elixir module looks like in erlang / core erlang",
      elixir: "~> 1.3",
      elixirc_paths: elixirc_paths(Mix.env()),
      package: package(),
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.json": :test
      ],
      source_url: "https://github.com/appunite/beam_inspect",
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls],
      version: @version
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:excoveralls, "~> 0.8.0", only: :test, runtime: false},
      {:ex_doc, "~> 0.13", only: :dev, runtime: false}
    ]
  end

  defp elixirc_paths(:dev), do: ["lib", "test/support"]
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp package do
    [
      maintainers: ["Tobiasz Małecki"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/appunite/beam_inspect"
      }
    ]
  end
end
