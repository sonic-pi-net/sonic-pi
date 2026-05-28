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

    if midi_enabled do
      Logger.info("Initialising MIDI native interface")
      :sp_midi.init()
    else
      Logger.info("Starting without MIDI native interface")
    end

    children = [
      :tau_server_sup
    ]

    opts = [strategy: :one_for_one, name: Tau.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
