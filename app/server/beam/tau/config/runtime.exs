import Config

log_path = case System.fetch_env("TAU_LOG_PATH") do
             {:ok, log_path} ->
               log_path
             _error ->
               "log/tau.log"
           end

config :tau,
  handle_otp_reports: true,
  handle_sasl_reports: true

config :logger,
  backends: [{LoggerFileBackend, :error_log}]

config :logger, :error_log,
  path: log_path,
  level: :info

phx_port_default = 8001
phx_port = case System.fetch_env("TAU_PHX_PORT") do
             {:ok, port_str} ->
               case Integer.parse(port_str) do
                 {port, ""} ->
                   port
                 _other ->
                   phx_port_default
               end
             _error ->
               phx_port_default
           end

config :tau, TauWeb.Endpoint,
  server: true,
  http: [ip: {127, 0, 0, 1}, port: phx_port]

if config_env() == :dev do
  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
    "pDakDMi+9PfJKYIHcdf7MGIog4NRPiuws5eUT6M6Kcg3Wad69hT+tVwOccyjfYJ"

  config :tau, TauWeb.Endpoint,
    secret_key_base: secret_key_base
end


# CONFIG/runtime.exs is executed for all environments, including
# during releases. It is executed after compilation and before the
# system starts, so it is typically used to load production configuration
# and secrets from environment variables or elsewhere. Do not define
# any compile-time configuration in here, as it won't be applied.
# The block below contains prod specific runtime configuration.
if config_env() == :prod do
  IO.puts "prod"
  # The secret key base is used to sign/encrypt cookies and other secrets.
  # A default value is used in config/dev.exs and config/test.exs but you
  # want to use a different value for prod and you most likely don't want
  # to check this value into version control, so we use an environment
  # variable instead.
  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      You can generate one by calling: mix phx.gen.secret
      """
  config :tau, TauWeb.Endpoint,
    secret_key_base: secret_key_base

  # ## Using releases
  #
  # If you are doing OTP releases, you need to instruct Phoenix
  # to start each relevant endpoint:
  #
  #     config :tau, TauWeb.Endpoint, server: true
  #
  # Then you can assemble a release by calling `mix release`.
  # See `mix help release` for more information.

  # ## Configuring the mailer
  #
  # In production you need to configure the mailer to use a different adapter.
  # Also, you may need to configure the Swoosh API client of your choice if you
  # are not using SMTP. Here is an example of the configuration:
  #
  #     config :tau, Tau.Mailer,
  #       adapter: Swoosh.Adapters.Mailgun,
  #       api_key: System.get_env("MAILGUN_API_KEY"),
  #       domain: System.get_env("MAILGUN_DOMAIN")
  #
  # For this example you need include a HTTP client required by Swoosh API client.
  # Swoosh supports Hackney and Finch out of the box:
  #
  #     config :swoosh, :api_client, Swoosh.ApiClient.Hackney
  #
  # See https://hexdocs.pm/swoosh/Swoosh.html#module-installation for details.
end
