require IEx

defmodule DocsWeb.PageLive do
  use DocsWeb, :live_view

  @impl true
  def mount(_params, %{"metadata" => metadata}, socket) do
    metadata = Enum.reject(metadata, fn {k, _v} -> k == :_build end)
    page_keys = Enum.into(metadata, %{}, fn {k, v} -> {k, Enum.flat_map(v, fn x -> Map.keys(x) end)} end)
    active_pages = Enum.into(metadata, %{}, fn {k, v} -> {k, hd(Enum.flat_map(v, fn x -> Map.keys(x) end))} end)
    content = Enum.find(metadata[:synths], fn p -> Enum.member?(Map.keys(p), :dull_bell)  end)
    %{dull_bell:  data} = content
    {:ok, assign(socket, metadata: metadata, active_tab: :synths, active_page: :dull_bell, active_pages: active_pages, page_keys: page_keys[:synths], content: data)}
  end

  @impl true
  def render(assigns) do
    Phoenix.View.render(DocsWeb.PageView, "page.html", assigns)
  end

  @impl true
  def handle_event("change_tab", %{"active_tab" => active_tab}, socket) do
    active_tab = String.to_atom(active_tab)
    page_keys = Enum.into(socket.assigns.metadata, %{}, fn {k, v} -> {k, Enum.flat_map(v, fn x -> Map.keys(x) end)} end)[active_tab]
    active_page = socket.assigns.active_pages[active_tab]
    pages = socket.assigns.metadata[active_tab]
    content = Enum.find(pages, fn p -> Enum.member?(Map.keys(p), active_page)  end)
    %{^active_page => data} = content
    {:noreply, assign(socket, active_tab: active_tab, active_pages: socket.assigns.active_pages, page_keys: page_keys, content: data)}
  end

  @impl true
  def handle_event("change_page", %{"active_page" => active_page}, socket) do
    active_tab = socket.assigns.active_tab
    pages = socket.assigns.metadata[active_tab]
    active_page = String.to_atom(active_page)
    content = Enum.find(pages, fn p -> Enum.member?(Map.keys(p), active_page)  end)
    %{^active_page => data} = content
    {:noreply, assign(socket, active_pages: %{socket.assigns.active_pages | active_tab => active_page}, content: data)}
  end
end
