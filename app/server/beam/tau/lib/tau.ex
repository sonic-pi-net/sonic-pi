defmodule Tau do
  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    Logger.info("All systems booting....")

    _tau_env                       = extract_env("TAU_ENV",                            :string, "prod")
    midi_enabled                   = extract_env("TAU_MIDI_ENABLED",                   :bool, false)
    link_enabled                   = extract_env("TAU_LINK_ENABLED",                   :bool, false)
    cues_on                        = extract_env("TAU_CUES_ON",                        :bool, true)
    osc_in_udp_loopback_restricted = extract_env("TAU_OSC_IN_UDP_LOOPBACK_RESTRICTED", :bool, true)
    midi_on                        = extract_env("TAU_MIDI_ON",                        :bool, false)
    link_on                        = extract_env("TAU_LINK_ON",                        :bool, false)
    osc_in_udp_port                = extract_env("TAU_OSC_IN_UDP_PORT",                :int,  5000)
    api_port                       = extract_env("TAU_API_PORT",                       :int,  5001)
    spider_port                    = extract_env("TAU_SPIDER_PORT",                    :int,  5002)
    daemon_port                    = extract_env("TAU_DAEMON_PORT",                    :int,  -1)
    keep_alive_port                = extract_env("TAU_KEEP_ALIVE_PORT",                :int,  -1)
    daemon_token                   = extract_env("TAU_DAEMON_TOKEN",                   :int,  -1)

    if midi_enabled do
      Logger.info("Initialising MIDI native interface")
      :sp_midi.init()
    else
      Logger.info("Starting without MIDI native interface")
    end

    if link_enabled do
      Logger.info("Initialising Link native interface")
      :sp_link.init()
    else
      Logger.info("Starting without Link native interface")
    end

    :tau_server_sup.set_application_env(
      midi_enabled,
      link_enabled,
      cues_on,
      osc_in_udp_loopback_restricted,
      midi_on,
      link_on,
      osc_in_udp_port,
      api_port,
      spider_port,
      daemon_port,
      keep_alive_port,
      daemon_token
    )

    # Although we don't use the supervisor name below directly,
    # it can be useful when debugging or introspecting the system.

    if (keep_alive_port == -1) do
      Logger.info("Not starting keepalive server as no daemon port value was given")
    else
      :tau_keepalive.start_link(keep_alive_port, daemon_port, daemon_token)
    end

    :tau_server_sup.start_link()

    Logger.info("Starting Phoenix server")
    Tau.Application.start(nil, nil)
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
