import Config

# CONFIG/runtime.exs is executed for all environments, including
# during releases. It is executed after compilation and before the
# system starts, so it is typically used to load production configuration
# and secrets from environment variables or elsewhere. Do not define
# any compile-time configuration in here, as it won't be applied.

extract_env = fn name, kind, default ->
  env_val = System.get_env(name)

  if !env_val do
    default
  else
    extracted =
      case kind do
        :int ->
          {val, ""} = Integer.parse(env_val)
          val

        :bool ->
          dc_val = env_val |> String.downcase() |> String.trim()
          dc_val != "false" and dc_val != "0"

        :string ->
          env_val
      end

    extracted
  end
end

config :tau,
  handle_otp_reports: true,
  handle_sasl_reports: true

if config_env() == :dev do
  config :logger,
    backends: [{LoggerFileBackend, :tau_file_log}, :console]
else
  config :logger,
    backends: [{LoggerFileBackend, :tau_file_log}]
end

config :logger, :tau_file_log,
  path: extract_env.("TAU_LOG_PATH", :string, "log/tau.log"),
  level: :info

# ## Using releases
#
# If you are doing OTP releases, you need to instruct Phoenix
# to start each relevant endpoint:
#
#     config :tau, TauWeb.Endpoint, server: true
#
# Then you can assemble a release by calling `mix release`.
# See `mix help release` for more information.

phx_port = extract_env.("TAU_PHX_PORT", :int, 8002)

config :tau,
  midi_on: extract_env.("TAU_MIDI_ON", :bool, false),
  midi_enabled: extract_env.("TAU_MIDI_ENABLED", :bool, false),
  link_enabled: extract_env.("TAU_LINK_ENABLED", :bool, false),
  cues_on: extract_env.("TAU_CUES_ON", :bool, true),
  osc_in_udp_loopback_restricted: extract_env.("TAU_OSC_IN_UDP_LOOPBACK_RESTRICTED", :bool, true),
  link_on: extract_env.("TAU_LINK_ON", :bool, false),
  osc_in_udp_port: extract_env.("TAU_OSC_IN_UDP_PORT", :int, 5000),
  api_port: extract_env.("TAU_API_PORT", :int, 5001),
  spider_port: extract_env.("TAU_SPIDER_PORT", :int, 5002),
  daemon_port: extract_env.("TAU_DAEMON_PORT", :int, -1),
  daemon_token: extract_env.("TAU_DAEMON_TOKEN", :int, -1),
  daemon_host: {127, 0, 0, 1},
  phx_port: phx_port

config :tau, TauWeb.Endpoint,
  server: true,
  http: [ip: {127, 0, 0, 1}, port: phx_port]

if config_env() == :dev do
  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      "pDakDMi+9PfJKYIHcdf7MGIog4NRPiuws5eUT6M6Kcg3Wad69hT+tVwOccyjfYJpDakDMi+9PfJKYIHcdf7MGIog4NRPiuws5eUT6M6Kcg3Wad69hT+tVwOccyjfYJ"

  config :tau, TauWeb.Endpoint, secret_key_base: secret_key_base
end

# The block below contains prod specific runtime configuration.
if config_env() == :prod do
  IO.puts("prod")
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

  config :tau, TauWeb.Endpoint, secret_key_base: secret_key_base
end
