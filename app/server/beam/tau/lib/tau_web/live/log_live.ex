defmodule TauWeb.LogLive do
  use TauWeb, :live_view

  @impl true


  def mount(_params, _session, socket) do
    TauWeb.Endpoint.subscribe "room:logs"
    {:ok, socket, temporary_assigns: [logs: []]}
  end



  @impl true
  def handle_info(%{topic: "room:logs", payload: state}, socket) do
    hash = :crypto.hash(:md5, Kernel.inspect(state)) |> Base.encode16
    {level, ts, msg, md} = state
    # {:noreply, assign(socket, logs: [socket.assigns.logs | inspect(state)])}
    {:noreply, assign(socket, logs: [{level, ts, msg, md, hash}])}
  end


  @impl true
  def render(assigns) do
    ~H"""
    <.container max_width="full">
      <div id="log-messages" phx-update="prepend" class="bg-black text-white flex flex-col space-y-0 ">

        <%= for {level, ts, msg, _md, hash} <- @logs do %>
<pre class="whitespace-pre pa0s" id={hash}>
  <code class={"#{tw_ts}"}><%=ts%></code> <code class={"#{tw_level(level)}"}><%=pad_level(level)%></code> <block class={"#{tw_msg}"}><%=pad_newlines(msg)%></block>
</pre>
        <% end %>

      </div>
    </.container>
    """
  end

  defp tw_ts do
    "bg-pink-600"
  end

  defp tw_msg do
    "text-gray-400"
  end

  defp tw_level(:info) do
    "text-green-500"
  end

  defp tw_level(:debug) do
    "text-blue-500"
  end

  defp tw_level(:warn) do
    "text-orange-500"
  end

  defp tw_level(:error) do
    "text-red-300"
  end

  defp pad_newlines(msg) do
    String.replace(msg, "\n", "\n                    ")
  end


  defp pad_level(level) do
    String.pad_trailing("[#{Atom.to_string(level)}]", 8)
  end
end
