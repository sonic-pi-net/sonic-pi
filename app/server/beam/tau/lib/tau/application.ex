defmodule Tau.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    Logger.info("All systems booting....")

    # MIDI moved to SuperSonic (Rust/midir); the sp_midi NIF has been removed.

    children = [
      :tau_server_sup
    ]

    opts = [strategy: :one_for_one, name: Tau.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
