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
require_relative "tau_comms"

module SonicPi
  # Spider-side wrapper over TauComms (transport to the Tau BEAM server):
  # timestamped OSC/MIDI scheduling, MIDI ports, cue server, cue dispatch.
  # Link lives in SonicPi::LinkAPI, not here.
  class TauAPI
    def initialize(ports, handlers)
      @tau_api_events = IncomingEvents.new
      @client_id = @tau_api_events.gensym("RubyTauAPI")
      @global_timewarp = 0

      @tau_comms = SonicPi::TauComms.new("127.0.0.1",
                                         ports[:tau_port],
                                         ports[:listen_to_tau_port])
      @external_osc_cue_handler = handlers[:external_osc_cue]
      # MIDI moved to SuperSonic (see midi_api.rb); Tau no longer carries it.

      add_incoming_api_handlers!

      block_until_tau_ready!
    end

    def tau_ready?
      @tau_comms.tau_ready?
    end
    def block_until_tau_ready!
      @tau_comms.block_until_tau_ready!
    end

    def send_osc_at(t, host, port, path, *args)
      m = @tau_comms.encoder.encode_single_message(path, args)
      api_send_at(t + @global_timewarp, "/send-after", host, port, SonicPi::OSC::Blob.new(m))
    end

    def start_stop_cue_server!(stop)
      @tau_comms.send("/stop-start-cue-server", !stop)
    end

    def cue_server_internal!(internal)
      @tau_comms.send("/osc-in-udp-loopback-restricted", !!internal)
    end

    def osc_flush!
      @tau_comms.send("/flush", "default")
    end

    def set_global_timewarp!(time)
      @global_timewarp = time.to_f / 1000.0
    end

    private

    def add_incoming_api_handlers!
      @tau_comms.add_method("/tau-api-reply") do |args|
        _gui_id = args[0]
        key = args[1]
        payload = args[2..-1]
        @tau_api_events.async_event(key, payload)
      end

      @tau_comms.add_method("/external-osc-cue") do |args|
        _gui_id = args[0]
        ip = args[0]
        port = args[1]
        address = args[2]
        osc_args = args[3..-1]
        @external_osc_cue_handler.call(Time.now, ip, port, address, osc_args)
      end
    end

    def api_rpc(path, *args)
      key = @tau_api_events.gensym(@client_id)
      prom = Promise.new
      @tau_api_events.oneshot_handler(key) do |payload|
        prom.deliver! payload
      end
      @tau_comms.send("/api-rpc",  *args.unshift(key, path))
      prom.get
    end

    def api_send_at(t, path, *args)
      args.map! do |arg|
        case arg
        when Numeric, String, SonicPi::OSC::Blob
          arg
        else
          arg.inspect
        end
      end
      @tau_comms.send_ts(t, path, *args)
    end


  end
end
