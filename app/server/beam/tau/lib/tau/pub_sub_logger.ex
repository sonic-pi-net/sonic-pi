defmodule Tau.PubSubLogger do
  @behaviour :gen_event

  @impl true
  def init(_args) do
    {:ok, []}
  end

  @impl true
  def handle_call(_, state) do
    {:ok, :ok, state}
  end

  @impl true
  def handle_event({level, _gl, {Logger, msg, {_date, time}, md}}, state) do
    {:erl_level, level} = List.keyfind(md, :erl_level, 0, {:erl_level, level})
    ts = IO.chardata_to_string(Logger.Formatter.format_time(time))
    msg = IO.chardata_to_string(msg)
    TauWeb.Endpoint.broadcast "room:logs", "msg", {level, ts, msg, md}
    {:ok, state}
  end

  def handle_event(_, state) do
    {:ok, state}
  end

  @impl true
  def handle_info(_, state) do
    {:ok, state}
  end

  @impl true
  def code_change(_old_vsn, state, _extra) do
    {:ok, state}
  end

  @impl true
  def terminate(_reason, _state) do
    :ok
  end
end
