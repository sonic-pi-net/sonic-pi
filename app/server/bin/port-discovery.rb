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
else
  puts "Unknown port name: #{ARGV[0]}.\nExpecting one of:\n* gui-send-to-server\n* gui-listen-to-server\n* server-send-to-gui\n* server-listen-to-gui\n* server-osc-cues\n* scsynth\n* scsynth-send"
end
