require IEx

defmodule DocsWeb.PageLive do
  use DocsWeb, :live_view

  @impl true
  def mount(_params, %{"metadata" => metadata}, socket) do
    metadata = Enum.reject(metadata, fn {k, _v} -> k == :_build end)
    page_titles = Enum.into(metadata, %{}, fn {k, v} -> {k, Enum.flat_map(v, fn x -> Map.keys(x) end)} end)
    active_pages = Enum.into(metadata, %{}, fn {k, v} -> {k, hd(Enum.flat_map(v, fn x -> Map.keys(x) end))} end)
    {:ok, assign(socket, metadata: metadata, active_tab: :fx, active_page: :bitcrusher, active_pages: active_pages, page_titles: page_titles[:fx], content: nil)}
  end

  @impl true
  def render(assigns) do
    Phoenix.View.render(DocsWeb.PageView, "page.html", assigns)
  end

  @impl true
  def handle_event("change_tab", %{"active_tab" => active_tab}, socket) do
    active_tab = String.to_atom(active_tab)
    page_titles = socket.assigns.metadata[active_tab]
    |> Enum.flat_map(&Map.keys/1)
    |> Enum.uniq

    {:noreply, assign(socket, active_tab: active_tab, active_pages: socket.assigns.active_pages, page_titles: page_titles)}
  end

  @impl true
  def handle_event("change_page", %{"active_page" => active_page}, socket) do
    active_tab = socket.assigns.active_tab
    pages = socket.assigns.metadata[active_tab]
    page = String.to_atom(active_page)
    content = Enum.find(pages, fn p -> Enum.member?(Map.keys(p), page)  end)
    %{^page => data} = content
    {:noreply, assign(socket, active_pages: %{socket.assigns.active_pages | active_tab => page}, content: data)}
  end

end
