defmodule Tau.MixProject do
  use Mix.Project

  def project do
    [
      app: :tau,
      version: "0.1.0",
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    if Mix.env() == :dev do
      [
        mod: {Tau.Application, []},
        extra_applications: [:logger, :runtime_tools, :os_mon]
      ]
    else
      [mod: {Tau.Application, []}, extra_applications: [:logger, :runtime_tools]]
    end
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:jason, "~> 1.4"},
      {:telemetry, "~> 1.0"},
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:logger_file_backend, "~> 0.0.13"},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false}
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  defp aliases do
    [
      setup: ["deps.get"],
      "setup.dev": [
        "local.hex --force",
        "local.rebar --force",
        "deps.get"
      ],
      "setup.prod": [
        "local.hex --force",
        "local.rebar --force",
        "deps.get"
      ],
      "tau.release": [
        "setup.prod",
        "release --overwrite"
      ]
    ]
  end
end
