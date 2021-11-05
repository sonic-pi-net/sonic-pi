import Config

log_path = case System.fetch_env("TAU_LOG_PATH") do
          :error ->
            "tau.log"
          any -> any
        end

config :tau,
  handle_otp_reports: true,
  handle_sasl_reports: true

config :logger,
  backends: [{LoggerFileBackend, :error_log}]

config :logger, :error_log,
  path: log_path,
  level: :info
