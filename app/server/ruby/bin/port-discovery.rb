#!/usr/bin/ruby -wKU


# Change these values to alter the ports
# Sonic Pi uses to send and receive messages
# at run time:

# Port which the GUI uses to send messages to the server:
gui_send_to_server = 4557

# Port which the GUI uses to listen to messages from the server:
gui_listen_to_server = 4558

# Port which the server uses to send messages to the GUI:
server_send_to_gui = 4558

# Port which the server uses to listen to messages from the GUI:
server_listen_to_gui = 4557

# Port which the SuperCollider server scsynth listens to:
# (scsynth will automatically send replies back to the port
# from which the message originated from)
scsynth = 4556

# Port which the server uses to send messages to scsynth
scsynth_send = 4556

# Port which the server uses to listen to messages which
# will automatically be converted to cue events:
server_osc_cues = 4559

# Port which the Erlang router listens to.
erlang_router = 4560

# Port which the server uses to send OSC messages representing
# output MIDI. This is used by osmid's o2m to listen to incoming
# OSC messages and then forward them on as standard MIDI messages
osc_midi_out = 4561


# Port which the server uses to listen to OSC messages generated
# by incoming MIDI. This is used by osmid's m2o as the outgoing
# port.
osc_midi_in = 4562



case (ARGV[0] || "").downcase
when "gui-send-to-server"
  puts gui_send_to_server
when "gui-listen-to-server"
  puts gui_listen_to_server
when "server-send-to-gui"
  puts server_send_to_gui
when "server-listen-to-gui"
  puts server_listen_to_gui
when "server-osc-cues"
  puts server_osc_cues
when "scsynth"
  puts scsynth
when "scsynth-send"
  puts scsynth_send
when "erlang-router"
  puts erlang_router
when "osc-midi-out"
  puts osc_midi_out
when "osc-midi-in"
  puts osc_midi_in
else
  puts "Unknown port name: #{ARGV[0]}.\nExpecting one of:\n* gui-send-to-server\n* gui-listen-to-server\n* server-send-to-gui\n* server-listen-to-gui\n* server-osc-cues\n* scsynth\n* scsynth-send"
end
