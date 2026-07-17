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
# The Daemon launches and watches over the following long-living
# processes (necessary for Sonic Pi to work):
#
#   +--> Scsynth - the SuperCollider audio engine
#   +--> Spider  - the Ruby Runtime server
#
# The Daemon does all the work necessary to figure out the correct
# process paths and flags - even considering config files such as
# `v5-audio-settings.toml`
#
#
# Zombie Kill Switch
# ------------------
#
# It is the Daemon's responsibility to ensure that the processes it
# boots are always terminated and are not left to turn into zombies and
# stick around continuing to accidentally run on your computer consuming
# resources.
#
# This is achieved via a "Zombie Kill Switch" - a UDP connection with an
# external process (such as the GUI) to monitor its status by
# continually receiving keep_alive messages. If these messages stop
# being received (for example, if the GUI process exited normally or
# even crashed) then the Daemon will ensure all the processes it spawned
# (Spider and Scsynth) are terminated.
#
# The port number of this kill switch UDP connection is printed to
# STDOUT. A external process (such as the GUI) must connect promptly and
# periodically send an OSC message with the path /daemon/keep-alive (more
# frequently than every 3s) otherwise a timeout will trigger the kill
# switch and kill all the spawned processes.
#
# If the client wants to explicitly trigger the kill switch directly
# rather than via a timeout it can send an OSC message with path
# /daemon/exit along with a single integer argument which is the comms
# token (also printed to STDOUT - see below).
#
#
# Port Allocations & Comms Token
# ------------------------------
#
# The Daemon figures out appropriate (and currently free) values for all
# the ports used by various processes within Sonic Pi to communicate
# with each other. These ports are used to create the correct process
# arguments for spawning and are also passed to STDOUT.
#
# Some of these port numbers need to be known by the client process
# so that it can both send code to run and receive log updates via UDP.
#
# The final value printed to stdout is the comms token which is a random
# 32 bit signed integer. This must be used as the first argument to all
# OSC messages sent from the GUI to Daemon such as: /daemon/keep-alive
# and /daemon/exit.
#
# The current allocations of these external port numbers are printed to
# STDOUT in the following order:
#
# daemon-keep-alive gui-listen-to-server gui-send-to-server scsynth osc-cues token
#
#
# Stdout Parameter Descriptions
# -----------------------------
#
# daemon:               UDP port Daemon is listening on. This is used for
#                       receiving /daemon/keep-alive OSC messages amongst
#                       other things.
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
# token:                32 bit signed integer used as a token to authenticate
#                       OSC messages.  All OSC messages sent from the GUI
#                       must include this token as the first argument


module SonicPi
  module Daemon
    class Init

      def initialize(opts={})
        @no_scsynth_inputs = opts[:no_scsynth_inputs]

        @exit_prom = Promise.new
        # use a value within the valid range for a 32 bit signed complement integer
        @daemon_token =  rand(-2147483647..2147483647)

        # Uncomment for debugging purposes
        # Util.log "Daemon token: #{@daemon_token}"

        @safe_exit = SafeExit.new do
          @exit_prom.deliver! true
          # Register exit routine
          # This will only be called once
          Util.log "----"
          Util.log "Selected ports: "
          Util.log @ports.inspect
          Util.log "Token: #{@daemon_token}"
          Util.log "----"
          Util.log "Daemon Booter is now exiting."
          Util.log "Cleaning up any running processes..."
          cleanup_any_running_processes
          Util.log "Daemon Booter - Over and Out."
          Util.close_log
        end

        # This is where the Daemon begins and ends.

        @spider_booter     = nil
        @compton_booter    = nil
        @supersonic_booter = nil

        if @no_scsynth_inputs
          Util.log "SuperSonic inputs disabled by GUI"
        else
          Util.log "SuperSonic inputs enabled by GUI"
        end

        #start compton to handle transparency (needs to be after Util.open_log)
        @compton_booter = ComptonBooter.new if Util.os == :raspberry

        # Get a map of port numbers to use
        #
        # Note that the program will safe_exit here
        # if there are problems detecting port numbers to use.
        @ports = PortDiscovery.new(@safe_exit).ports

        # Uncomment for debugging purposes
        # Util.log "Ports: #{@ports.inspect}"

        @kill_switch = KillSwitch.new(@safe_exit)

        @api_server = SonicPi::OSC::UDPServer.new(@ports["daemon"], suppress_errors: false, name: "Daemon API Server")
        # For debugging purposes:
        # @api_server = SonicPi::OSC::UDPServer.new(@ports["daemon"], suppress_errors: false, name: "Daemon API Server") do |address, args, sender_addrinfo|
        #   Util.log "Kill switch ##{@ports["daemon"] Received UDP data #{[address, args, sender_addrinfo].inspect}"
        # end

        @api_server.add_method("/daemon/keep-alive") do |args|
          if args[0] == @daemon_token
            @kill_switch.keep_alive!
          else
            Util.log "Kill switch for port #{@ports["daemon"]} received incorrect token. Ignoring #{args[0]}"
          end
        end

        @api_server.add_method("/daemon/exit") do |args|
          if args[0] == @daemon_token
            Util.log "[EXIT] Kill switch for port #{@ports["daemon"]} remotely activated using token #{@daemon_token}"
            @safe_exit.exit
          else
            Util.log "Kill switch for port #{@ports["daemon"]} received incorrect token. Ignoring #{args[0]}"
          end
        end

        # The sender to SuperSonic can be created before SuperSonic has
        # booted - it's just a UDP client pointed at a known port.
        @supersonic_sender = SonicPi::OSC::UDPClient.new('localhost', @ports["scsynth"])

        # Forward /supersonic/setup to Spider for cold-swap reinit
        @api_server.add_method("/supersonic/setup") do |args|
          Util.log "Forwarding /supersonic/setup to Spider"
          begin
            @api_server.send("localhost", @ports["gui-send-to-spider"], "/supersonic/setup", *args)
          rescue => e
            Util.log "Error forwarding /supersonic/setup: #{e.message}"
          end
        end

        ["/supersonic/statechange", "/supersonic/info", "/supersonic/devices", "/supersonic/input-devices", "/supersonic/devices/reopen.reply", "/supersonic/devices/reopen.done"].each do |path|
          @api_server.add_method(path) do |args|
            Util.log "Forwarding #{path} to GUI"
            begin
              @api_server.send("localhost", @ports["gui-listen-to-spider"], path, *args)
            rescue => e
              Util.log "Error forwarding #{path}: #{e.message}"
            end
          end
        end

        {
          "/daemon/audio/switch-device"   => "/supersonic/devices/switch",
          "/daemon/audio/switch-driver"   => "/supersonic/drivers/switch",
          "/daemon/audio/request-devices" => "/supersonic/devices/list",
          "/daemon/audio/reopen-device"   => "/supersonic/devices/reopen"
        }.each do |daemon_path, supersonic_path|
          @api_server.add_method(daemon_path) do |args|
            if args[0] && args[0] == @daemon_token
              Util.log "Forwarding #{daemon_path} to SuperSonic"
              begin
                @supersonic_sender.send(supersonic_path, *args[1..-1])
              rescue => e
                Util.log "Error forwarding #{daemon_path}: #{e.message}"
              end
            end
          end
        end

        # Let the calling process (likely the GUI) know which port to
        # listen to and communicate on with the Ruby spider server via
        # STDOUT. All ports and the token are known before SuperSonic
        # and Spider have booted, so send them now - this unblocks the
        # GUI to construct its UI concurrently with the server boot.
        # Boot failures are reported via /exited-with-boot-error OSC.
        puts "#{@ports["daemon"]} #{@ports["gui-listen-to-spider"]} #{@ports["gui-send-to-spider"]} #{@ports["scsynth"]} #{@ports["osc-cues"]} #{@daemon_token}"
        STDOUT.flush

        Util.log "Booting SuperSonic"
        @supersonic_booter = SupersonicBooter.new(@ports, @no_scsynth_inputs)

        # Spider boots concurrently — it self-syncs against SuperSonic
        # via its own /supersonic/notify ping loop.
        Util.log "Booting Spider Server"
        @spider_booter = SpiderBooter.new(@ports, @daemon_token)

        success = @supersonic_booter.wait_for_boot
        if success
          Util.log "SuperSonic booted successfully"
          # Send from @api_server so SuperSonic records its port as the notify target
          @api_server.send("localhost", @ports["scsynth"], "/supersonic/notify")
          Util.log "Sent /supersonic/notify to SuperSonic, registering daemon on port #{@ports["daemon"]}"
        else
          Util.log "sending ERROR to gui"
          @api_server.send("localhost", @ports["gui-listen-to-spider"], "/exited-with-boot-error", "SuperSonic Audio Server Boot Error\nSuperSonic failed to boot")
          @safe_exit.exit
        end

        Util.log "Blocking main thread until exit signal received..."
        begin
          @exit_prom.get
          Util.log "Exit signal received..."
        rescue
          # Way Out
        end

      end

      def cleanup_any_running_processes
        if @supersonic_sender && @supersonic_booter && @supersonic_booter.process_running?
          begin
            Util.log "Sending /quit to SuperSonic"
            @supersonic_sender.send("/quit")
          rescue => e
            Util.log "Error sending /quit: #{e.message}"
          end
        end

        [@spider_booter, @supersonic_booter, @compton_booter].map do |p|
          Thread.new do
            begin
              p.kill if p
            rescue StandardError => e
              Util.log "Error attempting to kill process #{p.inspect}"
              Util.log_error(e)
            end
          end
        end.each { |t| t.join }
      end
    end


    module Util
      def self.open_log
        begin
          @@log_file = File.open(Paths.daemon_log_path, 'a')
        rescue StandardError => e
          STDERR.puts "Unable to open log file #{Paths.daemon_log_path}"
          STDERR.puts e.inspect
          STDERR.puts "----\n\n"
          @@log_file = nil
        end
      end

      def self.close_log
        @@log_file.close if @@log_file
      end

      # Unified log line format across gui/spider/daemon/supersonic logs:
      # [HH:MM:SS.mmm] [tag] message. Logs are per-session (archived on each
      # boot) so the date is omitted; millis matter when debugging timing.
      def self.timestamp_for_log
        "[#{Time.now.strftime("%H:%M:%S.%3N")}]"
      end

      def self.log(msg)
        begin
          if @@log_file
            @@log_file.puts("#{timestamp_for_log} #{msg}")
            @@log_file.flush
          end
        rescue IOError => e
          STDERR.puts "Error. Unable to write to log file: #{e.message}"
          STDERR.puts e.inspect
        end
      end

      def self.log_error(e)
        spacer = "\n " + (" " * timestamp_for_log.size)
        log "#{e.class}"
        log "#{e.message}"
        log "##{e.backtrace.join(spacer)}"
      end

      def self.os
        case RUBY_PLATFORM
        when /.*linux.*/
          if File.exist?('/etc/rpi-issue')
            :raspberry
          else
            :linux
          end
        when /.*darwin.*/
          :macos
        when /.*mingw.*/
          :windows
        else
          raise "Unsupported platform #{RUBY_PLATFORM}"
        end
      end

      def self.pipewire?
        `which pw-link`
        $?.success?
      end
    end

    class KillSwitch
      def initialize(safe_exit)
        @safe_exit = safe_exit
        @kill_switch_prom = Promise.new
        @queue = Queue.new

        activate

        @timer_thread = Thread.new do
          attempts = 0
          max_attempts = 4
          Kernel.sleep 40
          loop do
            Kernel.sleep 10
            if @queue.empty?
              attempts += 1
            else
              attempts = 0
              @queue.clear
            end

            break if attempts > max_attempts
          end
          Util.log "Kill switch timed out..."
          @kill_switch_prom.deliver!(true)
        end
      end

      def keep_alive!
        @queue << true
      end

      def wait
        @kill_switch_prom.get
      end

      def activate
        return if @armed_thread
        @armed_thread = Thread.new do
          wait
          Util.log "[EXIT] Daemon kill switch triggered. Exiting..."
          @timer_thread.kill
          @safe_exit.exit
        end
      end

      def deactivate
        @armed_thread.kill
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
          Util.log "[EXIT] Daemon Process has completed:"

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
      attr_reader :pid, :args, :cmd, :log
      def initialize(cmd, args, log_path, record_log=false, env=nil, mirror_log=true)
        @env = env
        @pid = nil
        @log_file = nil
        @args = args.map {|el| el.to_s}
        @cmd = cmd
        @log = ""
        @record_log = record_log
        # Whether to also echo this process's output into the shared daemon log.
        # SuperSonic and Spider are already surfaced live in the GUI (shm debug
        # ring / OSC), so mirroring them just doubles their own log files as noise.
        @mirror_log = mirror_log
        @log_path = log_path
        @shutdown_requested = false
        @log_tail_logged = false
        @log_tail_mutex = Mutex.new
        if log_path
          begin
            @log_file = File.open(log_path, 'a')
          rescue StandardError => e
            STDERR.puts "Unable to open log file #{log_path}"
            STDERR.puts e.inspect
            STDERR.puts "----\n\n"
            @log_file = nil
          end
        end

        begin
          boot
        rescue StandardError => e
          Util.log "Error: something went wrong booting process: #{cmd}, #{args}, #{log_path}"
          Util.log_error(e)
          @log_file.close if @log_file
        end
      end

      def inspect
        "<ProcessBooter - cmd: #{@cmd}, pid: #{@pid.inspect}, args: #{@args.inspect}>"
      end

      def enable_internal_log_recording!
        @record_log = true
      end

      def disable_internal_log_recording!
        @record_log = false
      end

      def boot
        Util.log "Process Booter - booting #{@cmd}"

        # Uncomment for debugging
        # Util.log "Process Booter - booting #{@cmd} with args #{@args.inspect}"
        # Util.log "#{@cmd} #{@args.join(' ')}"
        if @env
          @stdin, @stdout_and_err, @wait_thr = Open3.popen2e @env, @cmd, *@args
        else
          @stdin, @stdout_and_err, @wait_thr = Open3.popen2e @cmd, *@args
        end
        @pid = @wait_thr.pid
        if @log_file
          @io_thr = Thread.new do
            @stdout_and_err.each do |line|
              begin
                line = line.force_encoding("UTF-8")
                # Stamp here rather than in each child process so every
                # per-process log file shares the unified line format.
                @log_file << "#{Util.timestamp_for_log} #{line}"
                @log_file.flush
                @log << line if @record_log
                Util.log "[#{File.basename(@cmd, ".*")}] #{line}" if @mirror_log
              rescue IOError
                # don't attempt to write
              end
            end
            # EOF here means the process has exited. If we didn't ask it
            # to, surface its final output in the daemon log for triage -
            # for non-mirrored processes the failure would otherwise only
            # be visible in the process's own log file.
            log_tail_to_daemon_log unless @shutdown_requested
          end
        end
      end

      # Append the tail of this process's own log file to the daemon log,
      # clearly labelled with the process name. Only needed for
      # non-mirrored processes - mirrored ones already have their output
      # in the daemon log. Idempotent, so it's safe to call from both the
      # unexpected-exit and failed-boot paths.
      def log_tail_to_daemon_log(num_lines=30)
        return if @mirror_log || !@log_path
        @log_tail_mutex.synchronize do
          return if @log_tail_logged
          @log_tail_logged = true
        end
        name = File.basename(@cmd, ".*")
        begin
          tail = File.readlines(@log_path).last(num_lines)
          Util.log "Last #{tail.size} lines of #{@log_path} for #{name}:"
          tail.each { |line| Util.log "[#{name}] #{line.chomp}" }
        rescue StandardError => e
          Util.log "Unable to read log file #{@log_path} for #{name}"
          Util.log_error(e)
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
        @shutdown_requested = true
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
      def initialize(ports, token)
        # RubyGems is not needed on the boot path (all deps are stdlib
        # or vendored) and skipping it shaves several hundred ms off
        # boot. Spider restores it before running any user code.
        args = [
          "--disable=gems",
          "--enable-frozen-string-literal", "-E", "utf-8", "--yjit",
          Paths.spider_server_path,
          "-u",
          ports["spider-listen-to-gui"],
          ports["spider-send-to-gui"],
          ports["scsynth"],
          ports["scsynth-send"],
          ports["osc-cues"],
          token
        ]

        # Spider's output is shown live in the GUI (OSC) and kept in spider.log;
        # don't also mirror it into the daemon log.
        super(Paths.ruby_path, args, Paths.spider_log_path, false, nil, false)
      end
    end


    class JackBooter < ProcessBooter
      def initialize
        cmd = "jackd"
        args = ["-T", "-d", "dummy", "-r", "48000", "-p", "1024"]
        super(cmd, args, Paths.jackd_log_path)
      end
    end

    class ComptonBooter < ProcessBooter
      def initialize
        cmd = "compton"
        args = []
        log_file = nil
        super(cmd, args, log_file)
      end
    end

    class SupersonicBooter < ProcessBooter

      # -i / -o omitted: SuperSonic auto-maxes to device channels.
      # TOML num_inputs / num_outputs still clamps them via OPTS_TOML_KEY_CONVERSION.
      DEFAULT_OPTS = {
        "-a" => "1024",
        "-m" => "131072",
        "-D" => "0",
        "-R" => "0",
        "-l" => "1",
        "-b" => "4096",
        "-B" => "127.0.0.1",
        "-Z" => "1024",
        # Sonic Pi's default tempo. Seeded at SuperSonic boot (not a post-boot
        # OSC tempo set) so bpm and beat origin are consistent from the first
        # clock read — SuperSonic's own default is 120.
        "--default-bpm" => "60"
      }.freeze

      OPTS_TOML_KEY_CONVERSION = {
        sound_card_name:          "-H",
        input_sound_card_name:    "__HI__",
        output_sound_card_name:   "__HO__",
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
        num_random_seeds:         "-r",
        audio_driver:             "--audio-driver"
      }.freeze

      # TOML keys handled outside OPTS_TOML_KEY_CONVERSION — suppress warnings
      KNOWN_NON_CLI_TOML_KEYS = [
        :linux_pipewire_buffsize,
        :linux_pipewire_samplerate
      ].freeze

      def initialize(ports, no_scsynth_inputs=false)
        @port = ports["scsynth"]

        if no_scsynth_inputs
          inputs_hash = {"-i" => "0"}
        else
          inputs_hash = {}
        end

        @boot_wait_mutex = Mutex.new

        begin
          toml_opts_hash = Tomlrb.load_file(Paths.user_audio_settings_path, symbolize_keys: true).freeze
        rescue StandardError => e
          Util.log "---- Audio Config Issue ----"
          if !File.exist? Paths.user_audio_settings_path
            Util.log "Could not find #{Paths.user_audio_settings_path}"
          else
            Util.log "Issue reading #{Paths.user_audio_settings_path}:"
            Util.log_error(e)
          end
          Util.log "Reverting to default audio options"
          Util.log "----------------------------"
          toml_opts_hash = {}
        end

        toml_opts_hash.freeze
        Util.log "Got Audio Settings toml hash: #{toml_opts_hash.inspect}"
        opts = unify_toml_opts_hash(toml_opts_hash)
        opts = inputs_hash.merge(opts)
        opts = {"-u" => @port}.merge(DEFAULT_OPTS).merge(opts)

        sound_card_name = opts.delete("-H")
        input_sound_card_name = opts.delete("__HI__")
        output_sound_card_name = opts.delete("__HO__")
        args = opts.to_a.flatten

        if input_sound_card_name && output_sound_card_name
          args << "-H" << input_sound_card_name << output_sound_card_name
        elsif input_sound_card_name && sound_card_name
          args << "-H" << input_sound_card_name << sound_card_name
        elsif input_sound_card_name
          args << "-H" << input_sound_card_name
        elsif output_sound_card_name
          args << "-H" << output_sound_card_name
        elsif sound_card_name
          args << "-H" << sound_card_name
        end

        # Point SuperSonic at the MdaPiano sample table (the :piano synth). The
        # table ships as an external asset rather than being compiled into the
        # engine; if it's missing, :piano just plays silence.
        if File.exist?(Paths.piano_wavetable_path)
          args << "--piano-wavetable" << Paths.piano_wavetable_path
        else
          Util.log "Piano wavetable not found at #{Paths.piano_wavetable_path} — :piano will be silent"
        end

        cmd = Paths.supersonic_path
        Util.log "SuperSonic opts: #{opts.inspect}"

        # Linux: set PIPEWIRE_QUANTUM from v5-audio-settings.toml
        env = nil
        if Util.os == :linux || Util.os == :raspberry
          pw_buf  = toml_opts_hash[:linux_pipewire_buffsize].to_i
          pw_rate = toml_opts_hash[:linux_pipewire_samplerate].to_i
          if pw_buf > 0 && pw_rate > 0
            quantum = "#{pw_buf}/#{pw_rate}"
            Util.log "Setting PIPEWIRE_QUANTUM=#{quantum} for SuperSonic"
            env = { "PIPEWIRE_QUANTUM" => quantum }
          elsif pw_buf > 0 || pw_rate > 0
            Util.log "v5-audio-settings.toml: linux_pipewire_buffsize and linux_pipewire_samplerate must both be set to apply PIPEWIRE_QUANTUM (got buf=#{pw_buf}, rate=#{pw_rate})"
          end
        end

        @success = Promise.new
        # SuperSonic's output is shown live in the GUI (shm debug ring) and kept
        # in supersonic.log; don't also mirror it into the daemon log.
        super(cmd, args, Paths.supersonic_log_path, false, env, false)
      end

      def wait_for_boot
        return @success.get if @success.delivered?
        @boot_wait_mutex.synchronize do
          return @success.get if @success.delivered?

          Util.log "Waiting for SuperSonic to have booted..."
          connected = false
          continue_pinging = true

          boot_s = OSC::UDPServer.new(0, name: "SuperSonic ack server") do |a, b, info|
            Util.log "Receiving ack from SuperSonic"
            @success.deliver! true unless connected
            continue_pinging = false
            connected = true
          end

          t = Thread.new do
            while continue_pinging
              begin
                if process_running?
                  Util.log "Sending /supersonic/notify to SuperSonic"
                  boot_s.send("localhost", @port, "/supersonic/notify")
                else
                  @success.deliver! false
                  continue_pinging = false
                end
              rescue Exception => e
                Util.log "Error sending to SuperSonic: #{e.message}"
              end
              sleep 0.25
            end
          end

          begin
            success = @success.get(30)
            if success
              Util.log "SuperSonic connection established"
              return true
            else
              Util.log "Unable to connect to SuperSonic"
              log_tail_to_daemon_log
              return false
            end
          rescue StandardError => e
            Util.log "Unable to connect to SuperSonic (#{e.message})."
            @success.deliver! false, false
            t.kill
            log_tail_to_daemon_log
            return false
          end
        end
      end

      private

      def unify_toml_opts_hash(toml_opts_hash)
        opts = {}
        toml_opts_hash.each do |k, v|
          v = case v
              when TrueClass then 1
              when FalseClass then 0
              when String then v.strip
              else v
              end
          key_sym = k.to_sym
          command_line_key = OPTS_TOML_KEY_CONVERSION[key_sym]
          if command_line_key
            opts[command_line_key] = v.to_s
          elsif !KNOWN_NON_CLI_TOML_KEYS.include?(key_sym)
            # Warn on keys origin/dev accepted (scsynth_opts*) but SuperSonic doesn't
            Util.log "Audio config: ignoring unknown key '#{k}' (value=#{v.inspect}) in #{Paths.user_audio_settings_path}"
          end
        end
        opts
      end
    end

    class PortDiscovery
      attr_reader :ports

      # Change these values to alter the ports
      # Sonic Pi uses to send and receive messages at run time:
      PORT_CONFIG = {
        # Port daemon uses to communicate with GUI or other controlling process
        "daemon" => :dynamic,

        # Port which the server uses to listen to messages from the GUI:
        "spider-listen-to-gui" => :dynamic,

        # Port which the GUI uses to send messages to the server:
        # May be paired with server_listen_to_gui
        "gui-send-to-spider" => :paired,

        # Port which the GUI uses to listen to messages from the server:
        "gui-listen-to-spider" => :dynamic,

        # Port which the server uses to send messages to the GUI:
        # May be paired with :gui_listen_to_server
        "spider-send-to-gui" => :paired,

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

        "spider" => :dynamic
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
          "spider-listen-to-gui",
          ["gui-send-to-spider","spider-listen-to-gui"],

          "gui-listen-to-spider",
          ["spider-send-to-gui", "gui-listen-to-spider"],

          "scsynth",
          ["scsynth-send", "scsynth"],

          "osc-cues",
          "spider",
          "daemon"].inject({}) do |res, port_name|

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
              Util.log "[EXIT] Invalid port default for port: #{port_name}. This port can not be paired."
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
            Util.log "[EXIT] Unable to find a free port."
            @safe_exit.exit
          end
        end
        @last_free_port
      end
    end
  end
end



begin
  SonicPi::Daemon::Util.open_log
  SonicPi::Daemon::Util.log "Welcome to the Daemon Booter"
  SonicPi::Daemon::Util.log "----------------------------\n"

  if ARGV[0] == "--no-scsynth-inputs"
    SonicPi::Daemon::Init.new(no_scsynth_inputs: true)
  else
    SonicPi::Daemon::Init.new
  end
rescue StandardError => e
  SonicPi::Daemon::Util.log "[BUG] - ** Daemon Internal Error. **"
  SonicPi::Daemon::Util.log_error(e)
else
  SonicPi::Daemon::Util.log "Daemon Finished. Cheerio."
end