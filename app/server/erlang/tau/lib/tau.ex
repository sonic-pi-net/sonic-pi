defmodule Tau do
use Application

  @impl true
  def start(_type, _args) do
    IO.puts "All systems booting.,.."

    enabled = extract_env("TAU_ENABLED", :bool)
    internal = extract_env("TAU_INTERNAL", :bool)
    midi_enabled = extract_env("TAU_MIDI_ENABLED", :bool)
    link_enabled = extract_env("TAU_LINK_ENABLED", :bool)
    in_port = extract_env("TAU_IN_PORT", :int)
    api_port = extract_env("TAU_API_PORT", :int)
    spider_port = extract_env("TAU_SPIDER_PORT", :int)

    Application.put_env(:tau, :enabled, enabled)
    Application.put_env(:tau, :internal, internal)
    Application.put_env(:tau, :midi_enabled, midi_enabled)
    Application.put_env(:tau, :link_enabled, link_enabled)
    Application.put_env(:tau, :in_port, in_port)
    Application.put_env(:tau, :api_port, api_port)
    Application.put_env(:tau, :spider_port, spider_port)

    :tau_server_sup.set_application_env(enabled,
      internal,
      midi_enabled,
      link_enabled,
      in_port,
      api_port,
      spider_port)

    # Although we don't use the supervisor name below directly,
    # it can be useful when debugging or introspecting the system.
    :tau_server_sup.start_link()
  end

  def extract_env(name, kind) do
    env_val = System.get_env(name)
    if !env_val do
      raise "Error, missing ENV Variable #{name}"
    end
    res = case kind do
            :bool -> extract_env_bool(env_val)
            :int -> extract_env_int(env_val)
            :string -> env_val

          end

    IO.puts "extracting env #{name} #{kind} #{res}"
    res
  end

  def extract_env_int(env_val) do
    {val, ""} = Integer.parse(env_val)
    val
  end

  def extract_env_bool(env_val) do
    dc_val = env_val |> String.downcase() |> String.trim()
    (dc_val != "false") and (dc_val != "0")
  end
end
