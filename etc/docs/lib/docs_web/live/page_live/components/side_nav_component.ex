require IEx

defmodule DocsWeb.PageLive.SideNavComponent do
  use DocsWeb, :live_component

  @impl true
  def render(assigns) do
    Phoenix.View.render(DocsWeb.PageView, "components/side_nav_component.html", assigns)
  end

  @impl true
  def update(%{active_page: active_page, active_tab: active_tab, active_pages: active_pages, page_keys: page_keys}, socket) do
    case active_tab do
      nil -> {:ok, assign(socket, active_pages: active_pages, page_keys: page_keys)}
      _ -> {:ok, assign(socket, active_page: active_page, active_pages: %{active_pages | active_tab => active_page}, page_keys: page_keys)}
    end
  end
end
