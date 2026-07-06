#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "util"
require_relative "supersonic_osc_comms"
require_relative "osc/timetag"

module SonicPi
  # Spider-side OSC API over SupersonicOscComms. Outgoing user OSC is scheduled
  # in SuperSonic's deferred-event scheduler via /schedule (timetagged in
  # SuperClock's domain, so it stays locked to scsynth audio); incoming external
  # OSC arrives as /external-osc-cue and feeds the cue system. The /osc/* OSC
  # surface lives in SuperSonic (OscControl).
  class OscAPI

    def initialize(supersonic_host, supersonic_port, osc_cues_port, handlers)
      @osc_comms = SonicPi::SupersonicOscComms.new(supersonic_host, supersonic_port)
      @external_osc_cue_handler = handlers[:external_osc_cue]
      @osc_cues_port = Integer(osc_cues_port)
      @global_timewarp = 0

      add_incoming_handlers!
    end

    # Subscribe to /osc notifications and bind the cue server. Must run once
    # SuperSonic is up: subscribing from the constructor races the engine's UDP
    # bind and the datagram is silently dropped.
    def osc_system_start!
      @osc_comms.subscribe_to_notifications!
      # Defaults: forwarding on, loopback-restricted on; the GUI overrides at
      # boot / on pref change via start_stop_cue_server! / cue_server_internal!.
      @osc_comms.send("/osc/cue-server/config", @osc_cues_port, 1, 1)
    end

    # Schedule an outgoing OSC message to host:port at spider time `t` (seconds).
    def send_osc_at(t, host, port, path, *args)
      inner = @osc_comms.encoder.encode_single_message(path, args)
      # Wrap as a self-routing "/osc/send <host> <port> <inner>", then schedule it.
      # On fire the scheduler re-ingests /osc/send through the same dispatch an
      # immediate send hits — the host/port ride inside the blob, not the wrapper.
      send_msg = @osc_comms.encoder.encode_single_message(
        "/osc/send", [host.to_s, Integer(port), SonicPi::OSC::Blob.new(inner)])
      tt = SonicPi::OSC.osc_timetag(t + @global_timewarp)
      @osc_comms.send("/schedule",
                      SonicPi::OSC::Int64.new(tt),
                      SonicPi::OSC::Blob.new(send_msg))
    end

    # Toggle whether inbound external OSC is forwarded as cues ("Enable OSC
    # server" pref). `stop` true → forwarding off.
    def start_stop_cue_server!(stop)
      @osc_comms.send("/osc/cue-server/cues-on", stop ? 0 : 1)
    end

    # Toggle loopback restriction ("Allow OSC from other computers" pref).
    # `internal` true → loopback only.
    def cue_server_internal!(internal)
      @osc_comms.send("/osc/cue-server/loopback", internal ? 1 : 0)
    end

    # Cancel pending scheduled OSC on run stop (the scheduler-level flush also
    # covers scheduled MIDI — see MidiAPI#midi_flush!).
    def osc_flush!
      @osc_comms.send("/sched/flush", "default")
    end

    def set_global_timewarp!(time)
      @global_timewarp = time.to_f / 1000.0
    end

    private

    def add_incoming_handlers!
      # External OSC re-framed by SuperSonic as
      #   /external-osc-cue <ip> <port> <address> <args...>
      # which the runtime cue handler consumes.
      @osc_comms.add_method("/external-osc-cue") do |args|
        ip       = args[0]
        port     = args[1]
        address  = args[2]
        osc_args = args[3..-1]
        @external_osc_cue_handler.call(Time.now, ip, port, address, osc_args) if @external_osc_cue_handler
      end
    end
  end
end
