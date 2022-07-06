# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

# Configures the endpoint
config :tau, TauWeb.Endpoint,
  url: [host: "localhost"],
  render_errors: [view: TauWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: Tau.PubSub,
  live_view: [signing_salt: "Zr1UgcpP"]

# Configure esbuild (the version is required)
config :esbuild,
  version: "0.14.25",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => System.get_env("MIX_DEPS_PATH") || Path.expand("../deps", __DIR__)}
  ],
  path: System.get_env("MIX_ESBUILD_PATH")

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

config :petal_components, :error_translator_function, {PetalBoilerplateWeb.ErrorHelpers, :translate_error}

config :tailwind,
  version: "3.0.23",
  default: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/app.css
      --output=../priv/static/assets/app.css
    ),
    cd: Path.expand("../assets", __DIR__)
  ],
  path: System.get_env("MIX_TAILWINDCSS_PATH")

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
