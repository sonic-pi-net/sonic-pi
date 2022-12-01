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

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

config :petal_components, :error_translator_function, {PetalBoilerplateWeb.ErrorHelpers, :translate_error}

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
