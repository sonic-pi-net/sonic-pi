defmodule Tau.MixProject do
  use Mix.Project

  def project do
    [
      app: :tau,
      version: "0.1.0",
      elixir: "~> 1.13",
      elixirc_paths: elixirc_paths(Mix.env()),
      elixirc_options: [debug_info: Mix.env() == :dev],
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [mod: {Tau.Application, []}, extra_applications: [:logger, :runtime_tools]]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:rustler, "~> 0.26.0"},
      {:logger_file_backend, "~> 0.0.13"},
      {:phoenix, "~> 1.6"},
      {:phoenix_html, "~> 3.0"},
      {:phoenix_live_view, "~> 0.18"},
      {:phoenix_live_dashboard, "~> 0.7"},
      # {:flame_on,               "~> 0.5.2"},
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:gettext, "~> 0.20"},
      {:jason, "~> 1.4"},
      {:plug_cowboy, "~> 2.6"},
      {:petal_components, "~> 0.18.0"},
      {:phoenix_live_reload, "~> 1.4", only: :dev},
      {:esbuild, "~> 0.5", runtime: Mix.env() == :dev, only: :dev},
      {:exsync, "~> 0.2.4", runtime: Mix.env() == :dev, only: :dev},
      {:tailwind, "~> 0.1", runtime: Mix.env() == :dev, only: :dev},
      {:floki, ">= 0.30.0", only: :test}
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to install project dependencies and perform other setup tasks, run:
  #
  #     $ mix setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    [
      setup: ["deps.get"],

      "assets.deploy.dev": [
        "phx.digest.clean --all",
        "tailwind default",
        "esbuild default --sourcemap=inline"],

      "assets.deploy.prod": [
        "phx.digest.clean --all",
        "tailwind default --minify",
        "esbuild default --minify",
        "phx.digest"],

      "setup.dev": [
        "local.hex --force",
        "local.rebar --force",
        "deps.get",
        "tailwind.install",
        "esbuild.install",
        "assets.deploy.prod"],

      "tau.release": [
        "local.hex --force",
        "local.rebar --force",
        "deps.get",
        "release --overwrite"]
    ]
  end
end
