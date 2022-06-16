defmodule Tau.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

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
    daemon_token                   = extract_env("TAU_DAEMON_TOKEN",                   :int,  -1)
    daemon_host = {127,0,0,1}

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
      daemon_token,
      daemon_host
    )

    Logger.info("Starting Phoenix server")

    children = [
      # Start the supervision tree from Erlang modules
      :tau_server_sup,
      # Start the Telemetry supervisor
      TauWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: Tau.PubSub},
      # Start the Endpoint (http/https)
      TauWeb.Endpoint
      # Start a worker by calling: Tau.Worker.start_link(arg)
      # {Tau.Worker, arg}
    ]

    opts = [strategy: :one_for_one, name: Tau.Supervisor]

    with {:ok, pid} <- Supervisor.start_link(children, opts) do
      Logger.add_backend(Tau.PubSubLogger)
      {:ok, pid}
    end
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    TauWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  defp extract_env(name, kind, default) do
    env_val = System.get_env(name)

    if !env_val do
      Logger.info("No env variable supplied for #{name} using default: #{default}")
      default
    else
      extracted = extract_env(env_val, kind)
      Logger.info("Extracting env #{name} #{kind}: #{extracted}")
      extracted
    end
  end

  defp extract_env(env_val, :int) do
    {val, ""} = Integer.parse(env_val)
    val
  end

  defp extract_env(env_val, :bool) do
    dc_val = env_val |> String.downcase() |> String.trim()
    (dc_val != "false") and (dc_val != "0")
  end

  defp extract_env(env_val, :string) do
    env_val
  end
end
