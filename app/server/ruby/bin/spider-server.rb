#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

## This is the Spider Server - the Sonic Pi language runtime.

require 'cgi'
require 'rbconfig'


require_relative "../core.rb"
require_relative "../paths"
require_relative "../lib/sonicpi/studio"

require_relative "../lib/sonicpi/server"
require_relative "../lib/sonicpi/util"
require_relative "../lib/sonicpi/osc/osc"
require_relative "../lib/sonicpi/lang/core"
require_relative "../lib/sonicpi/lang/midi"
require_relative "../lib/sonicpi/lang/ixi"
require_relative "../lib/sonicpi/lang/sound"
#require_relative "../lib/sonicpi/lang/pattern"
require_relative "../lib/sonicpi/runtime"

require 'multi_json'
require 'memoist'
require 'fileutils'

include SonicPi::Util

## This is where the server starts....
STDOUT.puts "Sonic Pi Spider Server booting..."
STDOUT.puts "The time is #{Time.now}"

## Ensure ~/.sonic-pi/* user config, history and setting directories exist
[ SonicPi::Paths.home_dir_path,
  SonicPi::Paths.project_path,
  SonicPi::Paths.log_path,
  SonicPi::Paths.cached_samples_path,
  SonicPi::Paths.config_path,
  SonicPi::Paths.system_store_path
].each do |d|
  ensure_dir(d)
end

## Just check to see if the user still has an old settings.json and if
## so, remove it. This is now stored in system_cache_store_path
old_settings_file_path = File.absolute_path("#{SonicPi::Paths.home_dir_path}/settings.json")
File.delete(old_settings_file_path) if File.exist?(old_settings_file_path)

## Move across any config example files if they don't
## exist so the user has a starting point for
## modifying them.


begin
  if File.exists?(SonicPi::Paths.original_init_path)
    if (File.exists?(SonicPi::Paths.init_path))
      STDOUT.puts "Warning, you have an older init.rb file in #{SonicPi::Paths.original_init_path} which is now being ignored as your newer config/init.rb file is being used instead. Consider deleting your old init.rb (perhaps copying anything useful across first)."
    else
      STDOUT.puts "Found init.rb in old location #{SonicPi::Paths.original_init_path}. Moving it to the new config directory #{SonicPi::Paths.init_path}."
      FileUtils.mv(SonicPi::Paths.original_init_path, init_path)
    end
  end
rescue Exception => e
  STDOUT.puts "Warning: exception when comparing new and original init.rb paths"
  STDOUT.puts "Error message received:\n-----------------------"
  STDOUT.puts e.message
  STDOUT.puts e.backtrace.inspect
  STDOUT.puts e.backtrace
end




Dir["#{SonicPi::Paths.user_config_examples_path}/*"].each do |f|
  basename = File.basename(f)
  full_config_path = File.absolute_path("#{SonicPi::Paths.config_path}/#{basename}")
  unless File.exist?(full_config_path)
    FileUtils.cp(f, full_config_path)
  end
end

## Select the primary GUI protocol
gui_protocol = case ARGV[0]
               when "-t"
                 # Qt GUI + tcp
                 :tcp
               when "-u"
                 # Qt GUI + udp
                 :udp
               else
                 :udp
               end

STDOUT.puts "Using primary protocol: #{gui_protocol}"
STDOUT.puts "Detecting port numbers..."


# Port which the server listens to messages from the GUI
# server-listen-to-gui
server_port = ARGV[1] ? ARGV[1].to_i : 4557

# Port which the GUI uses to listen to messages from the server:
# server-send-to-gui
gui_port = ARGV[2] ? ARGV[2].to_i : 4558

# Port which the SuperCollider server scsynth listens to:
# (scsynth will automatically send replies back to the port
# from which the message originated from)
# scsynth
scsynth_port = ARGV[3] ? ARGV[3].to_i : 4556

# Port to use to send messages to SuperCollider.
# Typically this is the same as scsynth_port, but
# may differ if there's a relay between them
# scsynth-send
scsynth_send_port = ARGV[4] ? ARGV[4].to_i : scsynth_port

# Port which the server listens to for external OSC messges
# which will be automatically converted to cues.
# server-osc-cues
osc_cues_port = ARGV[5] ? ARGV[5].to_i : 4560

# Port which the Erlang scheduler/router listens to.
# erlang-router
tau_port = ARGV[6] ? ARGV[6].to_i : 4561

listen_to_tau_port = ARGV[7] ? ARGV[7].to_i : 4562

token = ARGV[8] ? ARGV[8].to_i : 0

# Create a frozen map of the ports so that this can
# essentially be treated as a global constant to the
# language runtime.
sonic_pi_ports = {
  server_port: server_port,
  gui_port: gui_port,
  scsynth_port: scsynth_port,
  scsynth_send_port: scsynth_send_port,
  osc_cues_port: osc_cues_port,
  tau_port: tau_port,
  listen_to_tau_port: listen_to_tau_port}.freeze


STDOUT.puts "Ports: #{sonic_pi_ports.inspect}"
STDOUT.puts "Token: #{token}"
STDOUT.flush

# Open up comms to the GUI.
# We need to do this now so we can communicate with it going forwards
begin
  case gui_protocol
  when :tcp
    gui = SonicPi::OSC::TCPClient.new("127.0.0.1", gui_port, use_encoder_cache: true)
  when :udp
    gui = SonicPi::OSC::UDPClient.new("127.0.0.1", gui_port, use_encoder_cache: true)
  end

rescue Exception => e
  STDOUT.puts "Exception when opening socket to talk to GUI"
  case gui_protocol
  when :tcp
    STDOUT.puts "Attempted to use TCP on port #{gui_port}"
  when :udp
    STDOUT.puts "Attempted to use UDP on port #{gui_port}"
  end
  STDOUT.puts "Error message received:\n-----------------------"
  STDOUT.puts e.message
  STDOUT.puts e.backtrace.inspect
  STDOUT.puts e.backtrace
end


# Now we need to set up a server to listen to messages from the GUI.
begin
  case gui_protocol
  when :tcp
    osc_server = SonicPi::OSC::TCPServer.new(server_port, use_decoder_cache: true)
  when :udp
    STDOUT.puts "Opening UDP Server to listen to GUI on port: #{server_port}"
    osc_server = SonicPi::OSC::UDPServer.new(server_port, use_decoder_cache: true, name: "Spider API Server")
  end
rescue Exception => e
  begin
    STDOUT.puts "Exception when opening a socket to listen from GUI!"
    STDOUT.puts e.message
    STDOUT.puts e.backtrace.inspect
    STDOUT.puts e.backtrace
    STDOUT.flush
    gui.send("/exited-with-boot-error", "Failed to open server port " + server_port.to_s + ", is scsynth already running?")
  rescue Errno::EPIPE => e
    STDOUT.puts "GUI not listening, exit anyway."
    STDOUT.flush
  end
  exit
end

STDOUT.puts "Spider - Pulling in modules..."
STDOUT.flush

user_methods = Module.new
name = "SonicPiLang" # this should be autogenerated
klass = Object.const_set name, Class.new(SonicPi::Runtime)

klass.send(:include, user_methods)
klass.send(:include, SonicPi::Lang::Core)
klass.send(:include, SonicPi::Lang::Sound)
klass.send(:include, SonicPi::Lang::Midi)
klass.send(:include, SonicPi::Lang::Ixi)
klass.send(:include, SonicPi::Lang::Support::DocSystem)
klass.send(:extend, Memoist)

# This will pick up all memoizable fns in all modules as they share the
# same docsystem.
# TODO think of a better way to modularise this stuff when we move to
# using namespaces...

SonicPi::Lang::Core.memoizable_fns.each do |f|
  klass.send(:memoize, f)
end

klass.send(:define_method, :inspect) { "Runtime" }
#klass.send(:include, SonicPi::Lang::Pattern)

ws_out = Queue.new


begin
  STDOUT.puts "Spider - Starting Runtime Server"
  STDOUT.flush

  sp =  klass.new sonic_pi_ports, ws_out, user_methods

  STDOUT.puts "Spider - Runtime Server Initialised"
  STDOUT.flush
  # read in init.rb if exists
  if File.exists?(SonicPi::Paths.init_path)
    sp.__spider_eval(File.read(SonicPi::Paths.init_path), silent: true)
  else
    STDOUT.puts "Spider - Could not find init.rb file: #{SonicPi::Paths.init_path} "
  end

  sp.__print_boot_messages

rescue Exception => e
  STDOUT.puts "Spider - Failed to start server: " + e.message
  STDOUT.puts e.backtrace.join("\n")
  STDOUT.flush
  gui.send("/exited-with-boot-error", "Server Exception:\n #{e.message}\n #{e.backtrace}")
  exit
end

at_exit do
  STDOUT.puts "Spider - Server is exiting."
  begin
    STDOUT.puts "Spider - Shutting down GUI..."
    gui.send("/exited")
  rescue Errno::EPIPE => e
    STDOUT.puts "Spider - GUI not listening."
  end
  STDOUT.puts "Spider - Goodbye :-)"
  STDOUT.flush
end


register_api = lambda do |server|
  STDOUT.puts "Spider - Registering incoming Spider Server API endpoints"
  # server.add_global_method do | *args |
  #   STDOUT.puts "API In - #{args.inspect}"
  #   STDOUT.flush
  # end



  server.add_method("/run-code") do |args|
    incoming_token = args[0]
    if incoming_token == token
      code = args[1].force_encoding("utf-8")
      sp.__spider_eval code
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /run-code API call"
      STDOUT.flush
    end
  end

  server.add_method("/save-and-run-buffer") do |args|
    incoming_token = args[0]
    if incoming_token == token
      buffer_id = args[1]
      code = args[2].force_encoding("utf-8")
      workspace = args[3]
      sp.__save_buffer(buffer_id, code)
      sp.__spider_eval code, {workspace: workspace}
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /save-and-run-buffer API call"
      STDOUT.flush
    end
  end

  server.add_method("/save-buffer") do |args|
    incoming_token = args[0]
    if incoming_token == token
      buffer_id = args[1]
      code = args[2].force_encoding("utf-8")
      sp.__save_buffer(buffer_id, code)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /save-buffer API call"
      STDOUT.flush
    end
  end

  server.add_method("/exit") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.__exit
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /exit API call"
      STDOUT.flush
    end
  end

  server.add_method("/stop-all-jobs") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.__stop_jobs
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /stop-all-jobs API call"
      STDOUT.flush
    end
  end

  server.add_method("/load-buffer") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.__load_buffer args[1]
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /load-buffer API call"
      STDOUT.flush
    end
  end

  server.add_method("/buffer-newline-and-indent") do |args|
    incoming_token = args[0]
    if incoming_token == token
      id = args[1]
      buf = args[2].force_encoding("utf-8")
      point_line = args[3]
      point_index = args[4]
      first_line = args[5]
      sp.__buffer_newline_and_indent(id, buf, point_line, point_index, first_line)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /buffer-newline-and-indent API call"
      STDOUT.flush
    end
  end

  server.add_method("/buffer-section-complete-snippet-or-indent-selection") do |args|
    incoming_token = args[0]
    if incoming_token == token
      id = args[1]
      buf = args[2].force_encoding("utf-8")
      start_line = args[3]
      finish_line = args[4]
      point_line = args[5]
      point_index = args[6]
      sp.__buffer_complete_snippet_or_indent_lines(id, buf, start_line, finish_line, point_line, point_index)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /buffer-section-complete-snippet-or-indent-selection API call"
      STDOUT.flush
    end
  end

  server.add_method("/buffer-indent-selection") do |args|
    incoming_token = args[0]
    if incoming_token == token
      id = args[1]
      buf = args[2].force_encoding("utf-8")
      start_line = args[3]
      finish_line = args[4]
      point_line = args[5]
      point_index = args[6]
      sp.__buffer_indent_lines(id, buf, start_line, finish_line, point_line, point_index)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /buffer-indent-selection API call"
      STDOUT.flush
    end
  end

  server.add_method("/buffer-section-toggle-comment") do |args|
    incoming_token = args[0]
    if incoming_token == token
      id = args[1]
      buf = args[2].force_encoding("utf-8")
      start_line = args[3]
      finish_line = args[4]
      point_line = args[5]
      point_index = args[6]
      sp.__toggle_comment(id, buf, start_line, finish_line, point_line, point_index)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /buffer-section-toggle-comment call"
      STDOUT.flush
    end
  end

  server.add_method("/buffer-beautify") do |args|
    incoming_token = args[0]
    if incoming_token == token
      id = args[1]
      buf = args[2].force_encoding("utf-8")
      line = args[3]
      index = args[4]
      first_line = args[5]
      sp.__buffer_beautify(id, buf, line, index, first_line)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /buffer-beautify call"
      STDOUT.flush
    end
  end

  server.add_method("/ping") do |args|
    incoming_token = args[0]
    if incoming_token == token
      id = args[1]
      gui.send("/ack", id) if id
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /ping call"
      STDOUT.flush
    end
  end

  server.add_method("/start-recording") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.recording_start
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /start-recording call"
      STDOUT.flush
    end
  end

  server.add_method("/stop-recording") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.recording_stop
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /stop-recording call"
      STDOUT.flush
    end
  end

  server.add_method("/delete-recording") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.recording_delete
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /delete-recording call"
      STDOUT.flush
    end
  end

  server.add_method("/save-recording") do |args|
    incoming_token = args[0]
    if incoming_token == token
      filename = args[1]
      sp.recording_save(filename)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /save-recording call"
      STDOUT.flush
    end
  end

  server.add_method("/reload") do |args|
    incoming_token = args[0]
    if incoming_token == token
      dir = File.dirname("#{File.absolute_path(__FILE__)}")
      Dir["#{dir}/../lib/**/*.rb"].each do |d|
        load d
      end
      puts "Spider - reloaded"
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /reload call"
      STDOUT.flush
    end
  end

  server.add_method("/mixer-invert-stereo") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.set_mixer_invert_stereo!
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /mixer-invert-stereo call"
      STDOUT.flush
    end
  end

  server.add_method("/mixer-standard-stereo") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.set_mixer_standard_stereo!
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /mixer-standard-stereo call"
      STDOUT.flush
    end
  end

  server.add_method("/mixer-stereo-mode") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.set_mixer_stereo_mode!
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /mixer-stereo-mode call"
      STDOUT.flush
    end
  end

  server.add_method("/mixer-mono-mode") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.set_mixer_mono_mode!
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /mixer-mono-mode call"
      STDOUT.flush
    end
  end

  server.add_method("/mixer-hpf-enable") do |args|
    incoming_token = args[0]
    if incoming_token == token
      freq = args[1].to_f
      sp.set_mixer_hpf!(freq)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /mixer-hpf-enable call"
      STDOUT.flush
    end
  end

  server.add_method("/mixer-hpf-disable") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.set_mixer_hpf_disable!
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /mixer-hpf-disable call"
      STDOUT.flush
    end
  end

  server.add_method("/mixer-lpf-enable") do |args|
    incoming_token = args[0]
    if incoming_token == token
      freq = args[1].to_f
      sp.set_mixer_lpf!(freq)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /mixer-lpf-enable call"
      STDOUT.flush
    end
  end

  server.add_method("/mixer-lpf-disable") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.set_mixer_lpf_disable!
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /mixer-lpf-disable call"
      STDOUT.flush
    end
  end

  server.add_method("/mixer-amp") do |args|
    incoming_token = args[0]
    if incoming_token == token
      amp = args[1]
      silent = args[2] == 1
      sp.set_volume!(amp, true, silent)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /mixer-amp call"
      STDOUT.flush
    end
  end

  server.add_method("/enable-update-checking") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.__enable_update_checker
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /enable-update-checking call"
      STDOUT.flush
    end
  end

  server.add_method("/disable-update-checking") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.__disable_update_checker
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /disable-update-checking call"
      STDOUT.flush
    end
  end

  server.add_method("/check-for-updates-now") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.__update_gui_version_info_now
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /check-for-updates-now call"
      STDOUT.flush
    end
  end

  server.add_method("/version") do |args|
    incoming_token = args[0]
    if incoming_token == token
      v = sp.__current_version
      lv = sp.__server_version
      lc = sp.__last_update_check
      plat = host_platform_desc
      gui.send("/version", v.to_s, v.to_i, lv.to_s, lv.to_i, lc.day, lc.month, lc.year, plat.to_s)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /version call"
      STDOUT.flush
    end
  end

  server.add_method("/gui-heartbeat") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.__gui_heartbeat incoming_token
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /gui-heartbeat call"
      STDOUT.flush
    end
  end

  server.add_method("/midi-start") do |args|
    incoming_token = args[0]
    if incoming_token == token
      silent = args[1] == 1
      sp.__midi_system_start(silent)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /midi-start call"
      STDOUT.flush
    end
  end

  server.add_method("/midi-stop") do |args|
    incoming_token = args[0]
    if incoming_token == token
      silent = args[1] == 1
      sp.__midi_system_stop(silent)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /midi-stop call"
      STDOUT.flush
    end
  end


  server.add_method("/midi-reset") do |args|
    incoming_token = args[0]
    if incoming_token == token
      silent = args[1] == 1
      sp.__midi_system_reset(silent)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /midi-reset call"
      STDOUT.flush
    end
  end

  server.add_method("/cue-port-external") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.__cue_server_internal!(false)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /cue-port-external call"
      STDOUT.flush
    end
  end

  server.add_method("/cue-port-internal") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.__cue_server_internal!(true)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /cue-port-internal call"
      STDOUT.flush
    end
  end

  server.add_method("/cue-port-stop") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.__stop_start_cue_server!(true)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /cue-port-stop call"
      STDOUT.flush
    end
  end

  server.add_method("/cue-port-start") do |args|
    incoming_token = args[0]
    if incoming_token == token
      sp.__stop_start_cue_server!(false)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /cue-port-start call"
      STDOUT.flush
    end
  end

  server.add_method("/set-global-timewarp") do |args|
    incoming_token = args[0]
    if incoming_token == token
      time = args[1].to_f
      sp.__set_global_timewarp!(time)
    else
      STDOUT.puts "Invalid token: #{incoming_token} - ignoring /set-global-timewarp call"
      STDOUT.flush
    end
  end
end

register_api.call(osc_server)

# Send stuff out from Sonic Pi back out to osc_server
out_t = Thread.new do
  continue = true
  while continue
    begin
      message = ws_out.pop
      # message[:ts] = Time.now.strftime("%H:%M:%S")

      if message[:type] == :exit
        begin
          gui.send("/exited")
        rescue Errno::EPIPE => e
          STDOUT.puts "Spider - GUI not listening, exit anyway."
        end
        continue = false
      else
        case message[:type]
        when :incoming
          gui.send("/incoming/osc", message[:time], message[:id], message[:address], message[:args])
        when :multi_message
          gui.send("/log/multi_message", message[:jobid], message[:thread_name].inspect, message[:runtime].to_s, message[:val].size, *message[:val].flatten)
        when :midi_out_ports
          gui.send("/midi/out-ports", message[:val])
        when :midi_in_ports
          gui.send("/midi/in-ports", message[:val])
        when :link_num_peers
          gui.send("/link-num-peers", message[:val])
        when :link_bpm
          gui.send("/link-bpm", message[:val])
        when :info
          gui.send("/log/info", message[:style] || 0, message[:val] || "")
        when :syntax_error
          desc = message[:val] || ""
          line = message[:line] || -1
          error_line = message[:error_line] || ""
          desc = CGI.escapeHTML(desc)
          gui.send("/syntax_error", message[:jobid], desc, error_line, line, line.to_s)
        when :error
          desc = message[:val] || ""
          trace = message[:backtrace].join("\n")
          line = message[:line] || -1
          # TODO: Move this escaping to the Qt Client
          desc = CGI.escapeHTML(desc)
          trace = CGI.escapeHTML(trace)
          # puts "sending: /error #{desc}, #{trace}"
          gui.send("/error", message[:jobid], desc, trace, line)
        when "replace-buffer"
          buf_id = message[:buffer_id]
          content = message[:val] || "Internal error within a fn calling replace-buffer without a :val payload"
          line = message[:line] || 0
          index = message[:index] || 0
          first_line = message[:first_line] || 0
          #          puts "replacing buffer #{buf_id}, #{content}"
          gui.send("/buffer/replace", buf_id, content, line, index, first_line)
        when "replace-buffer-idx"
          buf_idx = message[:buffer_idx] || 0
          content = message[:val] || "Internal error within a fn calling replace-buffer-idx without a :val payload"
          line = message[:line] || 0
          index = message[:index] || 0
          first_line = message[:first_line] || 0
          #          puts "replacing buffer #{buf_id}, #{content}"
          gui.send("/buffer/replace-idx", buf_idx, content, line, index, first_line)
        when "run-buffer-idx"
          buf_idx = message[:buffer_idx] || 0
          #          puts "running buffer #{buf_idx}"
          gui.send("/buffer/run-idx", buf_idx)
        when "replace-lines"
          buf_id = message[:buffer_id]
          content = message[:val] || "Internal error within a fn calling replace-line without a :val payload"
          point_line = message[:point_line] || 0
          point_index = message[:point_index] || 0
          start_line = message[:start_line] || point_line
          finish_line = message[:finish_line] || start_line
          #          puts "replacing line #{buf_id}, #{content}"
          gui.send("/buffer/replace-lines", buf_id, content, start_line, finish_line, point_line, point_index)
        when :version
          v = message[:version]
          v_num = message[:version_num]
          lv = message[:latest_version]
          lv_num = message[:latest_version_num]
          lc = message[:last_checked]
          plat = host_platform_desc
          gui.send("/version", v.to_s, v_num.to_i, lv.to_s, lv_num.to_i, lc.day, lc.month, lc.year, plat.to_s)
        when :all_jobs_completed
          gui.send("/runs/all-completed")
        when :job
          id = message[:job_id]
          action = message[:action]
          # do nothing for now
        else
          STDOUT.puts "Spider - Ignoring #{message}"
          STDOUT.flush
        end

      end
    rescue Exception => e
      STDOUT.puts "Spider - Exception!"
      STDOUT.puts e.message
      STDOUT.puts e.backtrace.inspect
    end
  end
end

puts "Spider - Booted Successfully."
puts "Spider - #{sp.__current_version}, OS #{os}, on Ruby  #{RUBY_VERSION} | #{RbConfig::CONFIG['ruby_version']}."
puts "Spider - ------------------------------------------"


STDOUT.flush

out_t.join
