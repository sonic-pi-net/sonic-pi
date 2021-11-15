defmodule Tau.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      TauWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: Tau.PubSub},
      # Start the Endpoint (http/https)
      TauWeb.Endpoint
      # Start a worker by calling: Tau.Worker.start_link(arg)
      # {Tau.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Tau.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    TauWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
