#!/usr/bin/ruby -wKU
require 'socket'

# Change these values to alter the ports
# Sonic Pi uses to send and receive messages# at run time:
port_config = {
  # Port which the server uses to listen to messages from the GUI:
  "server-listen-to-gui" => :dynamic,

  # Port which the GUI uses to send messages to the server:
  # May be paired with server_listen_to_gui
  "gui-send-to-server" => :paired,

  # Port which the GUI uses to listen to messages from the server:
  "gui-listen-to-server" => :dynamic,

  # Port which the server uses to send messages to the GUI:
  # May be paired with :gui_listen_to_server
  "server-send-to-gui" => :paired,

  # Port which the SuperCollider server scsynth listens to:
  # (scsynth will automatically send replies back to the port
  # from which the message originated from)
  "scsynth" => :dynamic,

  # Port which the server uses to send messages to scsynth
  # May be paired with scsynth
  "scsynth-send" => :paired,

  # Port which the server uses to listen to messages which
  # will automatically be converted to cue events:
  "server-osc-cues" => 4560,

  # Port which the Erlang router listens to.
  "erlang-router" => :dynamic,

  # Port which the server uses to communicate via websockets
  "websocket" => :dynamic
}.freeze

check_port = lambda do |port|
  available = false
  begin
    socket = UDPSocket.new
    socket.bind('127.0.0.1', port)
    socket.close
    available = true
  rescue Exception
    available = false
  end
  available
end

last_free_port = 51234

find_free_port = lambda do
  while !check_port.call(last_free_port += 1)
    if last_free_port > 65535
      exit
    end
  end

  last_free_port
end

port_map = [
  # each entry is the name of a port to determine.
  # pairs of entry-names represent pairings where
  # the first element will default to the second
  # when its value is set to :paired
  "server-listen-to-gui",
  ["gui-send-to-server","server-listen-to-gui"],

  "gui-listen-to-server",
  ["server-send-to-gui", "gui-listen-to-server"],

  "scsynth",
  ["scsynth-send", "scsynth"],

  "server-osc-cues",
  "erlang-router",
  "websocket"].inject({}) do |res, port_name|

  default = nil
  case port_name
  when Array
    default = port_config[port_name[0]]
    if default == :dynamic
      port = find_free_port.call
    elsif default == :paired
      port = res[port_name[1]]
    else
      port = default
    end
    res[port_name[0]] = port.to_i
  else
    default = port_config[port_name]
    if default == :dynamic
      port = find_free_port.call
    elsif default == :paired
      puts "Invalid port default for port: #{port_name}. This port may not be paired."
      exit
    else
      port = default
      if(!check_port.call(port))
        port = find_free_port.call
      end
    end
    res[port_name] = port.to_i
  end

  res
end

port_map.each do |k, v|
  puts k.to_s + ": " + v.to_s
end
