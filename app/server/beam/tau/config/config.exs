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
  render_errors: [
    formats: [html: TauWeb.ErrorHTML, json: TauWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: Tau.PubSub,
  live_view: [signing_salt: "mZfPjIKs"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

if config_env() != :test do
  # Configure esbuild (the version is required)
  config :esbuild,
    version: "0.17.14",
    default: [
      args:
        ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/* --loader:.js=jsx),
      cd: Path.expand("../assets", __DIR__),
      env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
    ]

  # Configure tailwind (the version is required)
  config :tailwind,
    version: "3.3.0",
    default: [
      args: ~w(
      --config=tailwind.config.js
      --input=css/app.css
      --output=../priv/static/assets/app.css
    ),
      cd: Path.expand("../assets", __DIR__)
    ]
end

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

config :petal_components,
       :error_translator_function,
       {PetalBoilerplateWeb.ErrorHelpers, :translate_error}

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
