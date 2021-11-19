#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2021 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require 'socket'
require 'shellwords'
require 'open3'
require 'fileutils'
require 'time'
require 'securerandom'

require_relative "../lib/sonicpi/osc/osc"
require_relative "../lib/sonicpi/promise"

# Make sure vendored tomlrb lib is on the Ruby path so it can be required
Dir["#{File.expand_path("../../vendor", __FILE__)}/*/lib/"].each do |vendor_lib|
  $:.unshift vendor_lib
end

require 'tomlrb'
require_relative '../paths'

Thread::abort_on_exception = true

# The Sonic Pi Boot Daemon.
# =========================
#
# Process Spawning
# ----------------
#
# The Daemon launches and watches over the following
# long-living processes (necessary for Sonic Pi to work):
#
#   +--> Spider  - the Ruby Runtime server
#   +--> Tau     - the Erlang IO server
#   +--> Scsynth - the SuperCollider audio engine
#
# The Daemon does all the work necessary to figure out the correct
# process paths and flags - even considering config files such as
# `audio-settings.toml`
#
#
# Zombie Kill Switch
# ------------------
#
# It is the Daemon's responsibility to ensure that the processes it
# boots are always terminated and are not left to turn and stick around
# as zombies (processes still accidentally running on your computer
# consuming resources).
#
# This is achieved via a "Zombie Kill Switch" - a TCP connection with an
# external process (such as the GUI) to monitor its status by
# continually receiving keep_alive messages. If these messages stop
# being received (for example, if the GUI process exited normally or
# even crashed) then the Daemon will ensure all the processes it spawned
# (Spider, Tau and Scsynth) are terminated.
#
# The port number of this kill switch TCP connection is printed to
# STDOUT. A external process (such as the GUI) must connect promptly and
# periodicaly send some bytes (more frequently than every 5s) otherwise
# a timeout will trigger the kill switch and kill all the spawned
# processes.
#
#
# Port Allocation
# ---------------
#
# Boot Daemon figures out appropriate (and currently free) values for
# all the ports used by various processes within Sonic Pi to
# communicate with each other. These ports are used to create the
# correct process arguments for spawning and are also passed to STDOUT.
#
# Some of these port numbers need to be known by external processes -
# such as a GUI process that sends code to run and receives log updates both
# via UDP to specific ports.
#
# The current allocations of these external port numbers are printed to
# STDOUT in the following order:
#
# daemon-keep-alive gui-listen-to-server gui-send-to-server scsynth osc-cues
#
#
# External Port Descriptions
# --------------------------
#
# daemon-keep-alive:    TCP port Daemon uses to communicate with GUI (or
#                       other controlling process)
#
# gui-listen-to-server: UDP port which the GUI uses to listen to messages
#                       from the Spider Server.
#
# gui-send-to-server:   UDP port which the GUI uses to send messages to
#                       the Spider Server.
#
# scsynth:              UDP port on which scsynth listens (necessary for
#                       connecting to the boost shared memory for scope data)
#
# osc-cues:             UDP port used to receive OSC cue messages from external
#                       processes.
#
# phx:                  HTTP port used by the Phoenix web server


module SonicPi
  module Daemon
    class Init

      def initialize
        @safe_exit = SafeExit.new do
          # Register exit routine
          # This will only be called once
          Util.log "Daemon Booter is now exiting."
          Util.log "Cleaning up any running processes..."
          cleanup_any_running_processes
          Util.log "Daemon Booter - Over and Out."
          Util.close_log
        end

        # This is where the Daemon begins and ends.
        @scsynth_booter  = nil
        @tau_booter      = nil
        @spider_booter   = nil

        clear_logs

        Util.open_log
        Util.log "Welcome to the Daemon Booter"

        ports = PortDiscovery.new(@safe_exit).ports

        Util.log "Selected ports: "
        Util.log ports.inspect

        @zombie_kill_switch = spawn_zombie_kill_switch(ports["daemon-keep-alive"])


        # Boot processes
        Util.log "Booting Tau"
        begin
          @tau_booter = TauBooter.new(ports)
        rescue StandardError => e
          Util.log "Oh no, something went wrong booting Tau"
          Util.log "Error Class: #{e.class}"
          Util.log "Error Message: #{e.message}"
          Util.log "Error Backtrace: #{e.backtrace.inspect}"
        end

        # Let the calling process (likely the GUI) know which port to
        # listen to and communicate on with the Ruby spider server via
        # STDOUT.
        puts "#{ports["daemon-keep-alive"]} #{ports["gui-listen-to-server"]} #{ports["gui-send-to-server"]} #{ports["scsynth"]} #{ports["osc-cues"]} #{@tau_booter.phx_port}"
        STDOUT.flush


        Util.log "Booting Scsynth"
        @scsynth_booter = ScsynthBooter.new(ports)

        Util.log "Booting Spider Server"
        @spider_booter  = SpiderBooter.new(ports)

        # Wait for processes
        @tau_booter.wait if @tau_booter
        Util.log "Tau process has completed"

        @scsynth_booter.wait if @scsynth_booter
        Util.log "Scsynth process has completed"

        @spider_booter.wait if @spider_booter
        Util.log "Spider Server process has completed"
      end


      # This is the Zombie Kill Switch
      #
      # It is a separate thread which calls exit (therefore triggering
      # the at_exit block which in turn calls
      # cleanup_any_running_processes).
      #
      # There are the following exit conditions:
      #
      # Initial Connection Timeout - if nothing connects within 5 seconds,
      #                              switch is activated
      #
      # Connection Lost            - if the connection is broken or there's
      #                              a timeout - waiting for the next
      #                              keep-alive message.
      #
      def spawn_zombie_kill_switch(port_num, &blk)
        # Define how long to wait for an initial connection and between keep-alive messages.
        connect_timeout = 5
        recv_timeout = 30

        # Spawn TCP server
        keep_alive_server = TCPServer.new "127.0.0.1", port_num

        Thread.new do
          begin
            Util.log "Waiting for GUI on port #{port_num}...."

            unless IO.select([keep_alive_server], nil, nil, connect_timeout)
              Util.log "Critical Error. Unable to connect to GUI process on TCP port #{port_num}"
              @safe_exit.exit
            end

            client = keep_alive_server.accept
            Util.log "Connected to GUI!"

            while IO.select([client], nil, nil, recv_timeout) && received_data = client.gets
              # Don't do anything with incoming data.
              # Its presence signifies things are still OK.
              # For debug:
              # Util.log "RCV #{received_data}"
            end
          rescue Errno::ECONNRESET
            Util.log "GUI forcibly closed the connection."
          rescue StandardError => e
            Util.log "Oh no, something went wrong reading keep alive messages from the GUI"
            Util.log "Error Class: #{e.class}"
            Util.log "Error Message: #{e.message}"
            Util.log "Error Backtrace: #{e.backtrace.inspect}"
          end

          Util.log "Critical Error. Lost comms with GUI."
          client.close if client
          keep_alive_server.close if keep_alive_server

          @safe_exit.exit
        end
      end

      def cleanup_any_running_processes
        [@spider_booter, @scsynth_booter, @tau_booter].map do |p|
          Thread.new do
            begin
              p.kill if p
            rescue StandardError => e
              Util.log "Error attempting to kill process #{p.inspect}"
              Util.log e.class
              Util.log e.message
              Util.log e.backtrace.inspect
            end
          end
        end.each { |t| t.join }
      end


      # Copy contents of current logs to a rotating
      # backup directory and clear ready for a new
      # run.
      #
      # TODO: Handle the case where the log path isn't writable.
      def clear_logs

        # Windows doesn't allow certain chars in file paths
        # which are present in the default Time.now string format.
        # therefore remove them.
        sanitised_time_str = Time.now.to_s.gsub!(/[<>:|?*]/, '_')
        history_dir = File.absolute_path("#{Paths.log_history_path}/#{sanitised_time_str}")
        FileUtils.mkdir_p(history_dir)

        Dir["#{Paths.log_path}/*.log"].each do |p|
          # Copy log to history directory
          FileUtils.cp(p, "#{history_dir}/")
          # Clear out all logs (don't remove the files, just empty them)
          File.open(p, 'w') {|file| file.truncate(0) }
        end

        # clean up old history logs
        # only store the last 10 sessions
        num_sessions_to_store = 10

        history_dirs = Dir.glob("#{Paths.log_history_path}/*")

        timestamps = history_dirs.map do |d|
          begin
            Time.parse File.basename(d)
          rescue
            nil
          end
        end

        timestamps.compact!
        num_timestamps = timestamps.size
        num_to_drop = num_timestamps - num_sessions_to_store

        if num_to_drop.positive?
          timestamps.sort.take(num_to_drop).each do |ts|
            dir = ts.to_s
            FileUtils.rm_rf("#{Paths.log_history_path}/#{dir}")
          end
        end
      end
    end


    module Util
      def self.open_log
        begin
          @@log_file = File.open(Paths.daemon_log_path, 'a')
        rescue StandardError => e
          STDERR.puts "Unable to open log file #{Paths.daemon_log_path}"
          STDERR.puts e.inspect
          @@log_file = nil
        end
      end

      def self.close_log
        @@log_file.close if @@log_file
      end

      def self.log(msg)
        begin
          if @@log_file
            @@log_file.puts("[#{Time.now.strftime("%Y-%m-%d %H:%M:%S")}] #{msg}")
            @@log_file.flush
          end
        rescue IOError => e
          STDERR.puts "Error. Unable to write to log file: #{e.message}"
          STDERR.puts e.inspect
        end
      end

      def self.os
        case RUBY_PLATFORM
        when /.*arm.*-linux.*/
          :raspberry
        when /aarch64.*linux.*/
          :raspberry
        when /.*linux.*/
          :linux
        when /.*darwin.*/
          :macos
        when /.*mingw.*/
          :windows
        else
          raise "Unsupported platform #{RUBY_PLATFORM}"
        end
      end
    end

    class SafeExit

      def initialize(&cleanup_procedure)

        @exit_mut               = Mutex.new
        @exit_cleanup_mut       = Mutex.new
        @exit_in_progress       = false
        @exit_cleanup_completed = false
        @cleanup_procedure      = cleanup_procedure

        at_exit do
          @exit_mut.synchronize do
            @exit_in_progress = true
            idempotent_exit_cleanup
          end
        end
      end

      def exit
        Thread.current.kill if @exit_in_progress

        @exit_mut.synchronize do
          if @exit_in_progress
            Thread.current.kill
          else
            @exit_in_progress = true
            idempotent_exit_cleanup
            Kernel.exit
          end
        end
      end

      private

      def idempotent_exit_cleanup
        @exit_cleanup_mut.synchronize do
          unless @exit_cleanup_completed
            @cleanup_procedure.call
            @exit_cleanup_completed = true
          end
        end
      end
    end

    class ProcessBooter
      attr_reader :pid, :args, :cmd
      def initialize(cmd, args, log_path)
        @pid = nil
        @args = args.map {|el| el.to_s}
        @cmd = cmd
        if log_path
          @log_file = File.open(log_path, 'a')
          raise "Unable to create log file at path: #{log_path}" unless @log_file
        end

        begin
          boot
        rescue StandardError => e
          Util.log "Error: something went wrong booting process: #{cmd}, #{args}, #{log_path}"
          Util.log "Error Class: #{e.class}"
          Util.log "Error Message: #{e.message}"
          Util.log "Error Backtrace: #{e.backtrace.inspect}"
          @log_file.close if @log_file
        end
      end

      def inspect
        "<ProcessBooter - cmd: #{@cmd}, pid: #{@pid.inspect}, args: #{@args.inspect}>"
      end

      def boot
        Util.log "Process Booter - booting #{@cmd} with args #{@args.inspect}"
        Util.log "#{@cmd} #{@args.join(' ')}"
        @stdin, @stdout_and_err, @wait_thr = Open3.popen2e @cmd, *@args
        @pid = @wait_thr.pid
        if @log_file
          @io_thr = Thread.new do
            @stdout_and_err.each do |line|
              begin
                @log_file << line
                @log_file.flush
              rescue IOError
                # don't attempt to write
              end
            end
          end
        end
      end

      def process_running?
        return false unless @wait_thr
        @wait_thr.status
      end

      def wait
        begin
          @wait_thr.join if @wait_thr
        rescue Interrupt => e
          Util.log "Got interrupted waiting for #{@cmd} to complete"
        end
      end

      def kill

        if process_running? && @pid
          Util.log "Process Booter - killing #{@cmd} with pid #{@pid} and args #{@args.inspect}, wait_thr status: #{@wait_thr}, #{@wait_thr.status}"

          unless Util.os == :windows
            begin
              Util.log "Sending TERM kill command to #{@pid.inspect}"
              Process.kill("TERM", @pid)
            rescue Errno::ESRCH
              Util.log "Unable to send TERM kill command to #{@pid.inspect} as it's no longer running"
            end

            countdown = 5

            while process_running? && countdown >= 0
              Util.log "Process #{@pid.inspect} still running, waiting for it to finish... [#{countdown}]"
              sleep 1
              countdown -= 1
            end
          end

          if process_running?

            # We're either running on Windows (which doesn't seem to
            # support SIGTERM) or we attempted to kill the process nicely,
            # but unfortunately that didn't work, so let's forcefully kill
            # it
            begin
              Util.log "Sending KILL kill command to #{@pid.inspect}"
              Process.kill("KILL", @pid)
            rescue Errno::ESRCH
              Util.log "Unable to send KILL kill command to #{@pid.inspect} as it's no longer running"
            end

            countdown = 5

            while process_running? && countdown >= 0
              Util.log "Process #{@pid.inspect} still running, waiting for it to finish... [#{countdown}]"
              sleep 1
              countdown -= 1
            end
          end

          if process_running?
            Util.log "Unable to terminate process #{@pid.inspect}"
          else
            Util.log "Process #{@pid.inspect} terminated"
          end
        else
          Util.log "Process Booter - no need to kill #{@cmd} with pid #{@pid} and args #{@args.inspect} - already terminated, wait_thr status: #{@wait_thr}, #{@wait_thr.status}"
        end


        unless @pid
          Util.log "Process Booter - Unfortunately we don't have a @pid for  #{@cmd} with args #{@args.inspect}. wait_thr: #{@wait_thr}"
        end

        @io_thr.kill if @io_thr
        @log_file.close if @log_file

      end
    end

    class SpiderBooter < ProcessBooter
      def initialize(ports)
        server_listen_to_gui_port = ports["server-listen-to-gui"]
        server_send_to_gui_port   = ports["server-send-to-gui"]
        scsynth_port              = ports["scsynth"]
        scsynth_send_port         = ports["scsynth-send"]
        osc_cues_port             = ports["osc-cues"]
        tau_port                  = ports["tau"]
        listen_to_tau_port        = ports["listen-to-tau"]
        cmd = Paths.ruby_path
        args = ["--enable-frozen-string-literal", "-E", "utf-8",
          Paths.spider_server_path,
          "-u",
          server_listen_to_gui_port,
          server_send_to_gui_port,
          scsynth_port,
          scsynth_send_port,
          osc_cues_port,
          tau_port,
          listen_to_tau_port]
        super(cmd, args, Paths.spider_log_path)
      end
    end


    class TauBooter < ProcessBooter
      attr_reader :phx_port

      def initialize(ports)
        begin
          Util.log "fetching toml opts"
          toml_opts_hash = Tomlrb.load_file(Paths.user_tau_settings_path, symbolize_keys: true).freeze
          Util.log "got them: #{toml_opts_hash}"
          unified_opts = unify_tau_toml_opts(toml_opts_hash)
          Util.log "unified them: #{unified_opts}"
        rescue StandardError
          unified_opts = {}
        end

        @tau_pid                       = Promise.new
        cues_on                        = true
        osc_in_udp_loopback_restricted = false
        midi_on                        = true
        link_on                        = true
        osc_in_udp_port                = ports["osc-cues"]
        api_port                       = ports["tau"]
        spider_port                    = ports["listen-to-tau"]
        daemon_port                    = ports["daemon-listen-to-tau"]
        midi_enabled                   = true
        link_enabled                   = true
        @phx_port                      = unified_opts[:phx_port] || ports["phx"]
        phx_secret_key_base            = SecureRandom.base64(64)
        env                            = unified_opts[:env] || "prod"

        @osc_out_queue = SizedQueue.new(20)

        tau_comms    = TCPServer.new "127.0.0.1", daemon_port
        osc_decoder  = SonicPi::OSC::OscDecode.new
        osc_encoder  = SonicPi::OSC::OscEncode.new
        comms_thread_started = Promise.new

        Thread.new do
          client = nil

          @tau_comms_thread = Thread.new do
            comms_thread_started.deliver! true
            Util.log "----->   Accepting incoming connection from Tau"
            client = tau_comms.accept    # Wait for a client to connect
            Util.log "----->   Connection accepted"

            @tau_send_thread = Thread.new do
              loop do
                begin
                  osc = @osc_out_queue.pop
                  client.write([osc.bytesize].pack('N'))
                  client.write(osc)
                rescue StandardError => e
                  Util.log "Critical Error, sending messages to Tau:"
                  Util.log "Error Class: #{e.class}"
                  Util.log "Error Message: #{e.message}"
                  Util.log "Error Backtrace: #{e.backtrace.inspect}"
                end
              end
            end

            @tau_keep_alive_thread = Thread.new do
              loop do
                begin
                  osc = osc_encoder.encode_single_message("/system/keepalive")
                  @osc_out_queue << osc
                rescue StandardError => e
                  Util.log "Critical Error, encoding OSC keepalive message to Tau:"
                  Util.log "Error Class: #{e.class}"
                  Util.log "Error Message: #{e.message}"
                  Util.log "Error Backtrace: #{e.backtrace.inspect}"
                ensure
                  Kernel.sleep 4
                end
              end
            end

            recv_osc = lambda do
              size_str = client.recvfrom(4, Socket::MSG_WAITALL)[0].chomp
              size = size_str.unpack('N')[0]
              raise "Critical Error: Bad TCP OSC size received from Tau #{size_str.inspect}" unless size
              data_raw = client.recvfrom(size, Socket::MSG_WAITALL)[0].chomp
              osc_decoder.decode_single_message(data_raw)
            end

            begin
              data = recv_osc.call
              tau_pid = Integer(data[1][0])
              @tau_pid.deliver!(tau_pid)
              Util.log "----->   Tau Pid: #{tau_pid}"

              loop do
                data = recv_osc.call
                Util.log "Received OSC from Tau: #{data}"
                # In the future this is where we can handle any callbacks from Tau
              end
            rescue Errno::ECONNRESET
              Util.log  "Tau closed TCP connection"
            rescue StandardError => e
              Util.log "Critical Error, whilst communicating with Tau:"
              Util.log "Error Class: #{e.class}"
              Util.log "Error Message: #{e.message}"
              Util.log "Error Backtrace: #{e.backtrace.inspect}"
            end
          end

          @tau_comms_thread.join
          @tau_keep_alive_thread.kill
          @tau_send_thread.kill
          client.close if client
          tau_comms.close if tau_comms
        end

        comms_thread_started.get

        args = [
          cues_on,
          osc_in_udp_loopback_restricted,
          midi_on,
          link_on,
          osc_in_udp_port,
          api_port,
          spider_port,
          daemon_port,
          Paths.tau_log_path,
          midi_enabled,
          link_enabled,
          phx_port,
          phx_secret_key_base,
          env
        ]

        if Util.os == :windows
          cmd = Paths.mix_release_boot_path
        else
          cmd = "sh"
          args = [Paths.mix_release_boot_path] + args
        end

        super(cmd, args, nil)
      end

      def unify_tau_toml_opts(opts)
        unified_opts = {}

        # env should be either "dev" or "prod"
        case opts[:env].to_s.downcase.strip
        when "dev"
          unified_opts[:env] = "dev"
        when "prod"
          unified_opts[:env] = "prod"
        end

        # phx_port should be a positive integer
        begin
          phx_port = opts[:phx_port].to_i
          unified_opts[:phx_port] = phx_port if phx_port > 0
        rescue
          # do nothing
        end

        unified_opts.freeze
      end

      def process_running?
        @tau_comms_thread.alive? || super
      end

      def kill
        @tau_send_thread.kill
        @tau_keep_alive_thread.kill
        begin
          @pid = @tau_pid.get(30)
        rescue SonicPi::PromiseTimeoutError
          @pid = nil
          Util.log "Didn't receive Tau's Pid after waiting for 30s..."
        end
        @tau_comms_thread.kill
        super
      end

      def wait
        @tau_comms_thread.join
      end
    end


    class JackBooter < ProcessBooter
      def initialize
        cmd = "jackd"
        args = ["-T", "-d", "dummy", "-r", "48000", "-p", "1024"]
        super(cmd, args, Paths.jackd_log_path)
      end
    end


    class ScsynthBooter < ProcessBooter

      DEFAULT_OPTS = {
        "-a" => "1024",
        "-m" => "131072",
        "-D" => "0",
        "-R" => "0",
        "-l" => "1",
        "-i" => "16",
        "-o" => "16",
        "-b" => "4096",
        "-B" => "127.0.0.1" }.freeze

      OS_SPECIFIC_OPTS =
        case Util.os
        when :raspberry
          {
          "-c" => "128",
          "-z" => "128",
          "-i" => "2",
          "-o" => "2",
          "-U" => Paths.scsynth_raspberry_plugin_path
        }.freeze
        when :windows
          {
          "-U" => Paths.scsynth_windows_plugin_path
        }.freeze
        else
          {
        }.freeze
        end

      OPTS_TOML_KEY_CONVERSION = {
        sound_card_name:          "-H",
        sound_card_sample_rate:   "-S",
        sound_card_buffer_size:   "-Z",
        num_inputs:               "-i",
        num_outputs:              "-o",
        block_size:               "-z",
        enable_inputs:            "-I",
        enable_outputs:           "-O",
        num_control_bus_channels: "-c",
        num_audio_bus_channels:   "-a",
        num_sample_buffers:       "-b",
        max_num_nodes:            "-n",
        max_num_synthdefs:        "-d",
        real_time_memory_size:    "-m",
        num_wire_buffers:         "-w",
        num_random_seeds:         "-r"
      }.freeze


      def initialize(ports)
        @port = ports["scsynth"]
        begin
          toml_opts_hash = Tomlrb.load_file(Paths.user_audio_settings_path, symbolize_keys: true).freeze
        rescue StandardError
          toml_opts_hash = {}
        end

        opts = unify_toml_opts_hash(toml_opts_hash)
        opts = merge_opts(opts)
        @num_inputs = opts["-i"].to_i
        @num_outputs = opts["-o"].to_i
        args = opts.to_a.flatten
        cmd = Paths.scsynth_path
        run_pre_start_commands
        super(cmd, args, Paths.scsynth_log_path)
        run_post_start_commands
      end

      def kill
        @jack_booter.kill if @jack_booter
        super
      end

      def run_pre_start_commands
        case Util.os
        when :linux, :raspberry
          #Start Jack if not already running
          if `jack_wait -c`.include? 'not running'
            #Jack not running - start a new instance
            Util.log "Jackd not running on system. Starting..."
            @jack_booter = JackBooter.new
          else
            Util.log "Jackd already running. Not starting another server..."
          end
        end
      end

      def run_post_start_commands
        case Util.os
        when :linux, :raspberry
          Thread.new do
            Kernel.sleep 5
            # Note:
            # need to modify this to take account for @num_inputs and @num_outputs.
            # These might not always be set to two channels each.
            if @jack_booter
              #First clear up any pulseaudio remains of module-loopback source=jack_in
              `pactl list short modules |grep source=jack_in| cut -f1 | xargs -L1 pactl unload-module`
              `pactl load-module module-jack-source channels=2 connect=0 client_name=JACK_to_PulseAudio`
              `pactl load-module module-loopback source=jack_in`
              `pactl load-module module-jack-sink channels=2 connect=0 client_name=PulseAudio_to_JACK`
              `jack_connect PulseAudio_to_JACK:front-left SuperCollider:in_1`
              `jack_connect PulseAudio_to_JACK:front-right SuperCollider:in_2`
              `jack_connect SuperCollider:out_1 JACK_to_PulseAudio:front-left`
              `jack_connect SuperCollider:out_2 JACK_to_PulseAudio:front-right`
            else
              `jack_connect SuperCollider:out_1 system:playback_1`
              `jack_connect SuperCollider:out_2 system:playback_2`
              `jack_connect SuperCollider:in_1 system:capture_1`
              `jack_connect SuperCollider:in_2 system:capture_2`
            end
          end
        end
      end


      def unify_toml_opts_hash(toml_opts_hash)
        opts = {}

        toml_opts_hash.each do |k, v|
          v = case v
              when TrueClass
                1
              when FalseClass
                0
              when String
                v.strip
              else
                v
              end

          command_line_key = OPTS_TOML_KEY_CONVERSION[k.to_sym]
          val = v.to_s

          #raise "Unknown SuperCollider scsynth arg: #{k}. Expected one of #{OPTS_TOML_KEY_CONVERSION.keys.inspect}" unless command_line_key
          next unless command_line_key

          opts[command_line_key] = val
        end
        opts
      end

      def merge_opts(opts)
        # extract scsynth opts override
        begin
          clobber_opts_a = Shellwords.split(opts.fetch(:scsynth_opts_override, ""))
          scsynth_opts_override = clobber_opts_a.each_slice(2).to_h
        rescue
          scsynth_opts_override = {}
        end

        # extract scsynth opts
        begin
          scsynth_opts_a = Shellwords.split(opts.fetch(:scsynth_opts, ""))
          scsynth_opts = scsynth_opts_a.each_slice(2).to_h
        rescue
          scsynth_opts = {}
        end


        if scsynth_opts_override.empty?
          merged_opts = {"-u" => @port}.merge(DEFAULT_OPTS).merge(OS_SPECIFIC_OPTS).merge(opts).merge(scsynth_opts)

          # reduce number of inputs to 0 if inputs are disabled
          merged_opts["-i"] = 0 if merged_opts["-I"] == "0"

          # reduce number of outputs to 0 if outputs are disabled
          merged_opts["-o"] = 0 if merged_opts["-O"] == "0"

          return merged_opts
        else
          return scsynth_opts_override
        end
      end
    end

    class PortDiscovery
      attr_reader :ports

      # Change these values to alter the ports
      # Sonic Pi uses to send and receive messages at run time:
      PORT_CONFIG = {
        # Port daemon uses to communicate with GUI or other controlling process
        "daemon-keep-alive" => :dynamic,

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
        "osc-cues" => 4560,

        # Port which the Tau listens to.
        "tau" => :dynamic,

        # Port which the Ruby server listens to messages back from the Tau server
        "listen-to-tau" => :dynamic,

        "daemon-listen-to-tau" => :dynamic,

        # Port which the Phoenix webserver runs on
        "phx" => :dynamic
      }.freeze

      def initialize(safe_exit)
        @safe_exit = safe_exit
        # choose random port to try first
        @last_free_port = 29152 + rand(10000).to_i

        @ports = [
          # each entry is the name of a port to determine.
          # pairs of entry-names represent pairings where
          # the first element will default to the second
          # when its value is set to :paired
          "daemon-keep-alive",
          "server-listen-to-gui",
          ["gui-send-to-server","server-listen-to-gui"],

          "gui-listen-to-server",
          ["server-send-to-gui", "gui-listen-to-server"],

          "scsynth",
          ["scsynth-send", "scsynth"],

          "osc-cues",
          "tau",
          "listen-to-tau",
          "daemon-listen-to-tau",
          "phx"].inject({}) do |res, port_name|

          default = nil
          case port_name
          when Array
            default = PORT_CONFIG[port_name[0]]
            if default == :dynamic
              port = find_free_port
            elsif default == :paired
              port = res[port_name[1]]
            else
              if check_port(default)
                port = default
              else
                port = find_free_port
              end
            end
            res[port_name[0]] = port.to_i
          else
            default = PORT_CONFIG[port_name]
            if default == :dynamic
              port = find_free_port
            elsif default == :paired
              Util.log "Critical error: Invalid port default for port: #{port_name}. This port can not be paired."
              @safe_exit.exit
            else
              port = default
              if(!check_port(port))
                port = find_free_port
              end
            end
            res[port_name] = port.to_i
          end

          res
        end
      end

      def check_port(port)
        available = false
        begin
          socket = UDPSocket.new
          socket.bind('127.0.0.1', port)
          Util.log "checked port #{port}, #{socket}"
          socket.close
          available = true
        rescue StandardError
          available = false
        end
        available
      end

      def find_free_port
        while !check_port(@last_free_port += 1)
          if @last_free_port > 65535
            Util.log "Critical error: Unable to find a free port."
            @safe_exit.exit
          end
        end
        @last_free_port
      end
    end
  end
end

SonicPi::Daemon::Init.new
