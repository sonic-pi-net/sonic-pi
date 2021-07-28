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

require_relative "incomingevents"
require_relative "promise"
require_relative "util"

module SonicPi
  class TauAPI
    attr_reader :booted

    def initialize(ports)
      @tau_api_events = IncomingEvents.new
      @client_id = @tau_api_events.gensym("RubyTauAPI")
      @osc_cues_port = ports[:osc_cues_port]
      @tau_port = ports[:tau_port]
      @tau_host = "127.0.0.1"
      @listen_to_tau_port = ports[:listen_to_tau_port]
      @link_time = nil
      @local_time = nil
      @tau_comms = SonicPi::OSC::TauComms.new("127.0.0.1", @tau_port, @listen_to_tau_port)
      @external_osc_cue_handler = nil
      @internal_cue_handler = nil

      @tau_comms.add_method("/tau_api_reply") do |args|
        log "yo #{args}"
        gui_id = args[0]
        key = args[1]
        payload = args[2..-1]
        @tau_api_events.async_event(key, payload)
      end

      @tau_comms.add_method("/internal-cue") do |args|
        if @internal_cue_handler
          gui_id = args[0]
          path = args[1]
          args = args[2..-1]
          @internal_cue_handler.call(path, args)
        end
      end

      @tau_comms.add_global_method do |e, args|
        log "got incoming #{e}, #{args}"
      end

      @tau_comms.add_method("/external-osc-cue") do |args|
        if @external_osc_cue_handler
          gui_id = args[0]
          ip = args[0]
          port = args[1]
          address = args[2]
          osc_args = args[3..-1]
         @external_osc_handler.call(Time.now, ip, port, address, osc_args)
        end
      end

    end

    def osc_send(host, port, path, *args)
      m = @tau_comms.encoder.encode_single_message(path, args)
      __osc_send_api("/send_after", host, port, SonicPi::OSC::Blob.new(m))
    end

    def send_midi(path, *args)
      b = OSC::Blob.new(@tau_comms.encoder.encode_single_message(path, args))
      __osc_send_api("/midi_at", b)
    end

    def register_external_osc_cue_handler(&blk)
      @external_osc_cue_handler = blk
    end

    def register_cue_handler(&blk)
      @internal_cue_handler = blk
    end

    def api_rpc(path, *args, &blk)
      key = @tau_api_events.gensym(@client_id)
      prom = Promise.new
      @tau_api_events.oneshot_handler(key) do |payload|
        res = blk.call(payload)
        prom.deliver! res
      end
      @tau_comms.send(@tau_host, @tau_port, "/api_rpc",  *args.unshift(key, path))
      prom.get
    end

    def link_current_time
      api_rpc("/link_current_time") do |payload|
        payload[0].to_i
      end
    end

    def link_tempo
      api_rpc("/link_tempo") do |payload|
        payload[0].to_i
      end
    end

    def link_get_beat_at_time(time, quantum)
      api_rpc("/link_get_beat_at_time", time, quantum) do |payload|
        payload[0].to_i
      end
    end

    def start_stop_cue_server(stop)

      if stop
        @tau_comms.send(@tau_host, @tau_port, "/stop-start-cue-server", 0)
      else
        @tau_comms.send(@tau_host, @tau_port, "/stop-start-cue-server", 1)
      end
    end

    def cue_server_internal!(internal)
      if internal
        @tau_comms.send(@tau_host, @tau_port, "/internal-cue-port", 1)
      else
        @tau_comms.send(@tau_host, @tau_port, "/internal-cue-port", 0)
      end
    end

    def midi_flush!
      @tau_comms.send(@tau_host, @tau_port, "/midi_flush")
    end

    def osc_flush!
      @tau_comms.send(@tau_host, @tau_port, "/flush", "default")
    end

    def midi_system_start
      @tau_comms.send(@tau_host, @tau_port, "/stop-start-midi-cues", 1)
    end

    def midi_system_stop
      @tau_comms.send(@tau_host, @tau_port, "/stop-start-midi-cues", 0)
    end

    private

    def __osc_send_api(path, *args)
      t = __system_thread_locals.get(:sonic_pi_spider_time) + current_sched_ahead_time
      args.map! do |arg|
        case arg
        when Numeric, String, SonicPi::OSC::Blob
          arg
        else
          arg.inspect
        end
      end
      @tau_comms.send_ts(t, @tau_host, @tau_port, path, *args)
    end
  end
end
