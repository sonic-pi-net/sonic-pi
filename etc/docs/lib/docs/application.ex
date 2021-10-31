defmodule Docs.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      DocsWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: Docs.PubSub},
      # Start the Endpoint (http/https)
      DocsWeb.Endpoint
      # Start a worker by calling: Docs.Worker.start_link(arg)
      # {Docs.Worker, arg}
    ]

    :ets.new(:session, [:set, :public, :named_table, read_concurrency: true])
    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Docs.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    DocsWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
