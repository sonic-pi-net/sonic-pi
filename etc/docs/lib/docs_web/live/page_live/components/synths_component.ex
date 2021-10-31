defmodule DocsWeb.PageLive.SynthsComponent do
  use DocsWeb, :live_component

  @impl true
  def update(%{content: content}, socket) do
    {:ok, assign(socket, content: content)}
  end

  @impl true
  def render(assigns) do
    Phoenix.View.render(DocsWeb.PageView, "components/synths_component.html", assigns)
  end
end
