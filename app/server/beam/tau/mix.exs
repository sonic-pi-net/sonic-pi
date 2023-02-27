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
        extra_applications: [:logger, :runtime_tools, :file_system, :os_mon]
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
      {:phoenix, "~> 1.7.0"},
      {:phoenix_html, "~> 3.0"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:phoenix_live_view, "~> 0.18.4"},
      {:heroicons, "~> 0.5"},
      {:floki, ">= 0.30.0", only: :test},
      {:phoenix_live_dashboard, "~> 0.7.2"},
      {:esbuild, "~> 0.6", runtime: Mix.env() == :dev},
      {:tailwind, "~> 0.1.9", runtime: Mix.env() == :dev},
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:gettext, "~> 0.20"},
      {:jason, "~> 1.4"},
      {:plug_cowboy, "~> 2.6"},
      {:petal_components, "~> 1.0"},
      {:exsync, "~> 0.2", runtime: Mix.env() == :dev, only: :dev},
      {:rustler, "~> 0.26"},
      {:logger_file_backend, "~> 0.0.13"},
      {:credo, "~> 1.6", only: [:dev, :test], runtime: false}
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
        "esbuild default --sourcemap=inline"
      ],
      "assets.deploy.prod": [
        "phx.digest.clean --all",
        "tailwind default --minify",
        "esbuild default --minify",
        "phx.digest"
      ],
      "setup.dev": [
        "local.hex --force",
        "local.rebar --force",
        "deps.get",
        "tailwind.install",
        "esbuild.install",
        "assets.deploy.dev"
      ],
      "setup.prod": [
        "local.hex --force",
        "local.rebar --force",
        "deps.get",
        "tailwind.install",
        "esbuild.install",
        "assets.deploy.prod"
      ],
      "tau.release": [
        "setup.prod",
        "release --overwrite"
      ]
    ]
  end
end
