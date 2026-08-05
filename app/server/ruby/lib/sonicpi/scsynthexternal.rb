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

    # Raised when SuperSonic never answers the boot handshake. Distinct from a
    # missing process: the audio server may be running perfectly well and
    # simply unable to drain OSC because its audio device stopped ticking.
    class BootError < StandardError ; end

    attr_reader :version

    def initialize(events, scsynth_port, register_cue_event_lambda)
      @events = events
      @hostname = "127.0.0.1".freeze
      @send_port = scsynth_port
      @register_cue_event_lambda = register_cue_event_lambda
      raise "No cue event lambda!" unless @register_cue_event_lambda
      @out_queue = SizedQueue.new(20)
      @scsynth_thread_id = ThreadId.new(-5)
      @version = ""
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
      # Just unregister — the daemon owns SuperSonic's lifecycle
      begin
        @osc_server.send(@hostname, @send_port, "/supersonic/notify/unregister")
      rescue => e
        puts "Error unregistering from SuperSonic: #{e.message}"
      end
      @osc_server.stop
    end

    private

    def boot
      if booted?
        server_log "Server already booted..."
        return false
      end

      # TCP to the engine's command port (same number as the shm segment /
      # legacy UDP port — the daemon boots SuperSonic with `--tcp <port>`).
      # Connection establishment doubles as the boot handshake: the engine
      # only starts its stream transport once init (including a possibly
      # >10s Windows ASIO device open) has completed, so a successful
      # connect means the server is up — no ack polling needed.
      @osc_server = connect_to_server(60)
      # Engine-side notify registrations are per-connection: after any
      # reconnect they are gone and must be re-established before replies
      # (/done, /synced, /n_go…) flow again.
      @osc_server.on_reconnect { register_for_notifications!(timeout: 5.0) }

      @osc_server.add_global_method do |address, args, info|
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

      # Initial notify registration so /done, /synced, /n_go etc. flow
      # back to Spider. MUST happen before Server.new sends /d_loadDir.
      # Over TCP this also stands in for the old boot-ack handshake: the
      # reply carries the engine version and proves full req/reply flow.
      raise BootError, boot_timeout_message(30) unless register_for_notifications!(timeout: 30.0)

      true
    end

    # Register Spider as a /supersonic/notify target. Safe to call any
    # number of times — needed at boot AND after every driver-switch /
    # cold-swap because supersonic builds a fresh World whose subscribers
    # list is empty. Without this, /sync, /done and /n_go replies are
    # silently dropped and cold_swap_reinit's Phase 2 (/sync in
    # clear_scsynth!) and Phase 3 (/d_loadDir) hit their promise
    # timeouts. Returns true on confirmed re-registration, false on
    # timeout. Idempotent (re-binds the handler closure each call).
    #
    # Public: called by SonicPi::Server#register_for_notifications!
    # delegator from Studio#cold_swap_reinit!. (Targeted `public`
    # directive at end of body keeps subsequent methods private.)
    def register_for_notifications!(timeout: 5.0)
      return false unless @osc_server

      registered = Promise.new
      @osc_server.add_method("/supersonic/notify.reply") do |args|
        puts "Spider OSC: /supersonic/notify.reply confirmed"
        if args[1].is_a?(String) && !args[1].empty?
          @version = "v#{args[1]}".freeze
        end
        registered.deliver! true unless registered.delivered?
      end

      begin
        puts "Sending /supersonic/notify to register Spider comms server"
        @osc_server.send(@hostname, @send_port, "/supersonic/notify")
      rescue => e
        puts "Error sending /supersonic/notify: #{e.message}"
        registered.deliver! false
      end

      begin
        registered.get(timeout)
        true
      rescue
        puts "Warning: /supersonic/notify registration timed out (#{timeout}s)"
        false
      end
    end
    public :register_for_notifications!

    def connect_to_server(timeout)
      OSC::TcpOscClient.new(@hostname, @send_port,
                            use_decoder_cache: true,
                            use_encoder_cache: true,
                            name: "Scsynth Comms Server",
                            connect_timeout: timeout)
    rescue StandardError
      raise BootError, boot_timeout_message(timeout)
    end

    def raspberry?
      os == :raspberry
    end

    def boot_timeout_message(timeout)
      "The SuperSonic audio server did not respond within #{timeout} seconds.\n" \
        "It was started on port #{@send_port} but never answered the boot handshake.\n" \
        "This usually means the audio device is not responding - another\n" \
        "application may be holding it, or a virtual audio driver may be in a\n" \
        "bad state. Try selecting a different audio device, or restarting your\n" \
        "machine.\n" \
        "See #{Paths.log_path}/supersonic.log for what the audio server was doing."
    end

  end
end
