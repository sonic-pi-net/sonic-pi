defmodule DocsWeb.PageLive.ContentComponent do
  use DocsWeb, :live_component

  @impl true
  def update(%{content: content, active_tab: active_tab}, socket) do
    {:ok, assign(socket, content: content, active_tab: active_tab)}
  end

  @impl true
  def render(assigns) when assigns.active_tab == :fx or assigns.active_tab == :synths do
    #test = Map.take(assigns.content, [:doc])
    Phoenix.View.render(DocsWeb.PageView, "components/synths_or_fx_component.html", assigns)
  end

  # TODO: Store active_page per tab? this would allow us to check whether a page has been chosen on each tab,
  # and if not, render a default. (Also to render the last active page per tab when switching between them).
  @impl true
  def render(assigns) when assigns.active_tab == nil do
    Phoenix.View.render(DocsWeb.PageView, "components/welcome_component.html", %{})
  end
end
