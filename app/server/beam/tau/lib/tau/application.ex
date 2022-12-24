defmodule Tau.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    Logger.info("All systems booting....")

    midi_enabled = Application.get_env(:tau, :midi_enabled, false)
    link_enabled = Application.get_env(:tau, :link_enabled, false)

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
end
