defmodule DocsWeb.PageLive.SamplesComponent do
  use DocsWeb, :live_component

  @impl true
  def update(%{content: content}, socket) do
    {:ok, assign(socket, content: content)}
  end

  @impl true
  def render(assigns) do
    Phoenix.View.render(DocsWeb.PageView, "components/samples_component.html", assigns)
  end
end
