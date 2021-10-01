 import Config

config :tau,
  handle_otp_reports: true,
  handle_sasl_reports: true

config :logger,
  backends: [{LoggerFileBackend, :error_log}]

config :logger, :error_log,
  path: System.fetch_env!("TAU_LOG_PATH"),
  level: :info
