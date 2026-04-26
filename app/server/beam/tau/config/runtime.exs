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

config :logger,
  backends: [{LoggerFileBackend, :tau_file_log}]

config :logger, :tau_file_log,
  path: extract_env.("TAU_LOG_PATH", :string, "log/tau.log"),
  level: :info

if config_env() != :test do
  config :tau,
    midi_on: extract_env.("TAU_MIDI_ON", :bool, false),
    midi_enabled: extract_env.("TAU_MIDI_ENABLED", :bool, false),
    link_enabled: extract_env.("TAU_LINK_ENABLED", :bool, false),
    cues_on: extract_env.("TAU_CUES_ON", :bool, false),
    osc_in_udp_loopback_restricted:
      extract_env.("TAU_OSC_IN_UDP_LOOPBACK_RESTRICTED", :bool, true),
    link_on: extract_env.("TAU_LINK_ON", :bool, false),
    osc_in_udp_port: extract_env.("TAU_OSC_IN_UDP_PORT", :int, 5000),
    api_port: extract_env.("TAU_API_PORT", :int, 5001),
    spider_port: extract_env.("TAU_SPIDER_PORT", :int, 5002),
    daemon_port: extract_env.("TAU_DAEMON_PORT", :int, -1),
    daemon_token: extract_env.("TAU_DAEMON_TOKEN", :int, -1),
    daemon_host: {127, 0, 0, 1}
else
  config :tau,
    midi_on: extract_env.("TAU_MIDI_ON", :bool, false),
    midi_enabled: extract_env.("TAU_MIDI_ENABLED", :bool, false),
    link_enabled: extract_env.("TAU_LINK_ENABLED", :bool, false),
    cues_on: extract_env.("TAU_CUES_ON", :bool, true),
    osc_in_udp_loopback_restricted:
      extract_env.("TAU_OSC_IN_UDP_LOOPBACK_RESTRICTED", :bool, true),
    link_on: extract_env.("TAU_LINK_ON", :bool, false),
    osc_in_udp_port: extract_env.("TAU_OSC_IN_UDP_PORT", :int, Enum.random(30000..65535)),
    api_port: extract_env.("TAU_API_PORT", :int, Enum.random(30000..65535)),
    spider_port: extract_env.("TAU_SPIDER_PORT", :int, Enum.random(30000..65535)),
    daemon_port: extract_env.("TAU_DAEMON_PORT", :int, -1),
    daemon_token: extract_env.("TAU_DAEMON_TOKEN", :int, -1),
    daemon_host: {127, 0, 0, 1}
end
