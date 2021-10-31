defmodule DocsWeb.PageLive.SynthsOrFxComponent do
  use DocsWeb, :live_component

  @impl true
  def update(%{content: content}, socket) do
    {:ok, assign(socket, content: content)}
  end

  @impl true
  def render(assigns) do
    Phoenix.View.render(DocsWeb.PageView, "components/synths_or_fx_component.html", assigns)
  end
end
