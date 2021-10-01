defmodule Tau do
  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    Logger.info("All systems booting....")
    enabled      = extract_env("TAU_ENABLED",      :bool, true)
    internal     = extract_env("TAU_INTERNAL",     :bool, true)
    midi_enabled = extract_env("TAU_MIDI_ENABLED", :bool, true)
    link_enabled = extract_env("TAU_LINK_ENABLED", :bool, true)
    in_port      = extract_env("TAU_IN_PORT",      :int,  5000)
    api_port     = extract_env("TAU_API_PORT",     :int,  5001)
    spider_port  = extract_env("TAU_SPIDER_PORT",  :int,  5002)
    daemon_port  = extract_env("TAU_DAEMON_PORT",  :int,  -1)

    :tau_server_sup.set_application_env(enabled,
      internal,
      midi_enabled,
      link_enabled,
      in_port,
      api_port,
      spider_port,
      daemon_port
    )

    # Although we don't use the supervisor name below directly,
    # it can be useful when debugging or introspecting the system.

    if (daemon_port == -1) do
      Logger.info("Not starting keepalive server as no daemon port value was given")
    else
      :tau_keepalive.start_link(daemon_port)
    end

    :tau_server_sup.start_link()
  end

  def extract_env(name, kind, default) do
    env_val = System.get_env(name)
    res = if !env_val do
      Logger.info( "No env variable supplied for #{name} using default: #{default}")
      default
    else
      extracted = case kind do
                    :bool -> extract_env_bool(env_val)
                    :int -> extract_env_int(env_val)
                    :string -> env_val
                  end
      Logger.info( "extracting env #{name} #{kind} #{extracted}")
      extracted
    end
    res
  end

  def extract_env_int(env_val) do
    {val, ""} = Integer.parse(env_val)
    val
  end

  def extract_env_bool(env_val) do
    dc_val = env_val |> String.downcase() |> String.trim()
    (dc_val != "false") and (dc_val != "0")
  end
end
