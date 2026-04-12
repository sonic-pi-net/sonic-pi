#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative "util"
require_relative "promise"
require_relative "osc/osc"
require_relative "thread_id"
require_relative "../../paths"

require 'fileutils'
require 'shellwords'


module SonicPi
  class SCSynthExternal
    include Util

    attr_reader :version

    def initialize(events, scsynth_port, register_cue_event_lambda)
      @events = events
      @hostname = "127.0.0.1".freeze
      @send_port = scsynth_port
      @register_cue_event_lambda = register_cue_event_lambda
      raise "No cue event lambda!" unless @register_cue_event_lambda
      @out_queue = SizedQueue.new(20)
      @scsynth_thread_id = ThreadId.new(-5)
      @version = request_version.freeze
      boot
    end

    def sys(cmd)
      log "System: #{cmd}"
      system cmd
    end

    def send(*all_args)
      address, *args = *all_args
      log "OSC             ~ #{address} #{args.inspect}" if osc_debug_mode
      @osc_server.send(@hostname, @send_port, address, *args)
    end

    def send_at(ts, *all_args)
      address, *args = *all_args
      if osc_debug_mode
        if (a = __get_spider_time) && (b = __get_spider_start_time)
          vt = a - b
        elsif st = __get_spider_start_time
          vt = ts - st
        else
          vt = -1
        end
        log "BDL #{'%11.5f' % vt} ~ [#{vt}:#{ts.to_f}] #{address} #{args.inspect}"
      end

      @osc_server.send_ts(ts, @hostname, @send_port, address, *args)
    end

    def reboot
      shutdown
      boot
    end

    def booted?
      !!@scsynth_pid
    end

    def shutdown
      # Note: Spider does NOT send /quit to supersonic.
      # The daemon owns supersonic's lifecycle and sends /quit
      # during full shutdown via cleanup_any_running_processes.
      # Unregister from notify targets so SuperSonic stops broadcasting to us.
      puts "SCSynthExternal.shutdown called from:"
      puts caller.first(5).map { |c| "  #{c}" }.join("\n")
      STDOUT.flush
      begin
        @osc_server.send(@hostname, @send_port, "/supersonic/notify/unregister")
      rescue => e
        puts "Error unregistering from SuperSonic: #{e.message}"
      end
      puts "Stopping OSC server..."
      @osc_server.stop
      puts "Stopped OSC server..."
      t1, t2 = nil, nil
      t1.join if t1
      t2.join if t2

      #close log file if it's open
      @scsynth_log_file.close if @scsynth_log_file
      @scsynth_log_file = nil
    end

    private

    def request_version
      version_string = `"#{Paths.supersonic_path}" -v`
      m = version_string.match(/\A\s*(?:supersonic|scsynth)\s+([0-9.a-zA-Z-]+)\s.*/)
      if m && m[1] && !m[1].empty?
        "v#{m[1]}"
      else
        ""
      end
    end

    def boot
      if booted?
        server_log "Server already booted..."
        return false
      end

      @osc_server = OSC::UDPServer.new(0, use_decoder_cache: true, use_encoder_cache: true, name: "Scsynth Comms Server")

      @osc_server.add_global_method do |address, args, info|
        # Diagnostic: log all scsynth replies during boot and cold swap
        puts "Spider OSC [port #{@osc_server.port}]: #{address} #{args.inspect}" if address.start_with?('/done', '/synced', '/n_go', '/fail', '/supersonic')

        case address
        when '/n_end'
          id = args[0].to_i
          @events.async_event ['/n_end/', id], args
        when '/n_off'
          id = args[0].to_i
          @events.async_event ['/n_off/', id], args
        when '/n_on'
          id = args[0].to_i
          @events.async_event ['/n_on/', id], args
        when '/n_go'
          id = args[0].to_i
          @events.async_event ['/n_go/', id], args
        when '/n_move'
          id = args[0].to_i
          @events.async_event ['/n_move/', id], args
        else
          @events.async_event address, args
        end
        p = 0
        d = 0
        b = 0
        m = 60
        @register_cue_event_lambda.call(Time.now, p, @scsynth_thread_id, d, b, m, address, args) if address == "/supersonic/statechange" || address == "/supersonic/setup"
      end

      wait_for_boot

      # Register Spider's main comms server for push notifications.
      # Must wait for the reply before proceeding — Server.new will
      # immediately send /d_loadDir, and the /done reply goes to
      # registered notify targets. If we're not registered yet, we
      # miss the reply and timeout.
      registered = Promise.new
      @osc_server.add_method("/supersonic/notify.reply") do |args|
        puts "Spider OSC: /supersonic/notify.reply confirmed"
        registered.deliver! true
      end

      begin
        puts "Sending /supersonic/notify to register Spider comms server"
        @osc_server.send(@hostname, @send_port, "/supersonic/notify")
      rescue => e
        puts "Error sending /supersonic/notify: #{e.message}"
        registered.deliver! false
      end

      begin
        registered.get(5)
      rescue
        puts "Warning: /supersonic/notify registration timed out"
      end

      true
    end

    def raspberry?
      os == :raspberry
    end

    def wait_for_boot
      puts "SuperSonic boot - Waiting for audio server..."
      p = Promise.new
      connected = false

      boot_s = OSC::UDPServer.new(0, name: "SuperSonic ack server") do |a, b, info|
        puts "SuperSonic boot - Receiving ack"
        p.deliver! true unless connected
        connected = true
      end

      t = Thread.new do
        __system_thread_locals.set_local(:sonic_pi_local_thread_group, :scsynth_external_boot_ack)
        Kernel.loop do
          begin
            puts "SuperSonic boot - Sending /supersonic/notify"
            boot_s.send(@hostname, @send_port, "/supersonic/notify")
          rescue Exception => e
            puts "SuperSonic boot - Error: #{e.message}"
          end
          sleep 1
        end
      end

      begin
        p.get(30)
      rescue Exception => e
        puts "SuperSonic boot - Unable to connect (#{e.message}). Exiting..."
        exit
      ensure
        t.kill
        boot_s.stop
      end

      unless connected
        puts "SuperSonic boot - Unable to connect"
        raise "SuperSonic boot - Unable to connect"
      end

      puts "SuperSonic boot - Connection established"
    end

  end
end
