defmodule TauWeb.MainLive do
  use TauWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    TauWeb.Endpoint.subscribe("room:hydra")
    {:ok, push_event(socket, "hydra-code", %{hydra_code: "hydra.speed = 0.01;
    hydra.osc(5.9, 1, 1).colorama(1.2).add(hydra.noise(1.0)).out(hydra.o0);"})}
  end

  @impl true
  def render(assigns) do
    ~H"""

    """
  end

  @impl true
  def handle_info(%{topic: "room:hydra", payload: update}, socket) do
    {code} = update
    {:noreply, push_event(socket, "hydra-code", %{hydra_code: code})}
  end

  @impl true
  def handle_event("press", _value, socket) do
    # https://elixirforum.com/t/phoenix-liveview-request-proposal-mouse-events/31213/11
    send(:tau_server_cue, {:cue_debug, "phx-button-pressed"})
    {:noreply, socket}
  end

end
