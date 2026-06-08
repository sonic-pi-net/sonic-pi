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
require_relative "supersonic_midi_comms"

module SonicPi
  # Spider-side MIDI API over SupersonicMidiComms. Replaces sp_midi + the Tau
  # MIDI layer (tau_server_midi*.erl): outgoing MIDI is scheduled in SuperSonic's
  # deferred-event scheduler via /midi/at (timetagged in SuperClock's domain, so
  # it stays locked to scsynth audio); incoming MIDI arrives as /midi/in/* and is
  # re-emitted as the same /midi:<port>:<chan>/<event> cues Sonic Pi already uses.
  # The /midi/* OSC surface lives in SuperSonic (MidiControl + the Rust subsystem).
  class MidiAPI

    # NTP epoch (seconds 1900→1970) — matches OscEncode#time_encoded and the
    # engine's ntp_to_osc_timetag, so a MIDI event timetagged here lands on the
    # same clock as a scsynth bundle scheduled for the same time.
    NTP_OFFSET = 2208988800

    # Tau out-path → SuperSonic /midi/* address.
    OUT_MAP = {
      "/note_on"         => "/midi/out/note_on",
      "/note_off"        => "/midi/out/note_off",
      "/control_change"  => "/midi/out/control_change",
      "/aftertouch"      => "/midi/out/poly_pressure",   # poly key pressure
      "/channel_pressure"=> "/midi/out/channel_pressure",
      "/pitch_bend"      => "/midi/out/pitch_bend",
      "/program_change"  => "/midi/out/program_change",
      "/raw"             => "/midi/out/raw",
      "/sysex"           => "/midi/out/sysex",
      "/clock"           => "/midi/out/clock",
      "/clock_beat"      => "/midi/clock/beat",
      "/start"           => "/midi/out/start",
      "/stop"            => "/midi/out/stop",
      "/continue"        => "/midi/out/continue",
    }.freeze

    # Out-paths whose 2nd arg is the channel (so channel '*' → -1 fans to 1..16).
    CHANNEL_VOICE = %w[
      /note_on /note_off /control_change /aftertouch
      /channel_pressure /pitch_bend /program_change
    ].freeze

    def initialize(supersonic_host, supersonic_port, handlers)
      @midi_comms = SonicPi::SupersonicMidiComms.new(supersonic_host, supersonic_port)
      @internal_cue_handler = handlers[:internal_cue]
      @updated_midi_ins_handler = handlers[:updated_midi_ins]
      @updated_midi_outs_handler = handlers[:updated_midi_outs]
      @global_timewarp = 0

      add_supersonic_midi_handlers!
      @midi_comms.subscribe_to_notifications!
      # sp_midi opened every port; preserve that — open all in + out.
      @midi_comms.send("/midi/in/enable", "*", 1)
      @midi_comms.send("/midi/out/enable", "*", 1)
      @midi_comms.send("/midi/ports/list")   # prime the device lists
    end

    # Schedule outgoing MIDI at spider time `t` (seconds). Mirrors the old
    # TauAPI#send_midi_at signature so lang/midi.rb is a one-line repoint.
    def midi_send_at(t, tau_path, *args)
      ss_addr = OUT_MAP[tau_path]
      return unless ss_addr

      if CHANNEL_VOICE.include?(tau_path) && args[1] == -1
        (1..16).each do |ch|
          a = args.dup
          a[1] = ch
          send_one(t, ss_addr, a)
        end
      else
        send_one(t, ss_addr, args)
      end
    end

    def midi_system_start!
      @midi_comms.subscribe_to_notifications!
    end

    def midi_system_stop!
      @midi_comms.unsubscribe_from_notifications!
    end

    # No queued-send backlog to flush (events live in the engine scheduler).
    def midi_flush!; end

    def midi_refresh_devices!
      @midi_comms.send("/midi/refresh")
    end

    def set_global_timewarp!(time)
      @global_timewarp = time.to_f / 1000.0
    end

    private

    def send_one(t, ss_addr, args)
      inner = @midi_comms.encoder.encode_single_message(ss_addr, args)
      tt = osc_timetag(t + @global_timewarp)
      @midi_comms.send("/midi/at",
                       SonicPi::OSC::Int64.new(tt),
                       SonicPi::OSC::Blob.new(inner))
    end

    # Seconds (spider/SuperClock domain) → 64-bit OSC timetag.
    def osc_timetag(t)
      secs, frac = (t.to_f + NTP_OFFSET).divmod(1)
      (secs.to_i << 32) | (frac * 4294967296.0).to_i
    end

    # Re-emit a /midi/in/* push as the Sonic Pi cue Tau used to produce:
    #   /midi:<port>:<chan>/<event>  (channel events)
    #   /midi:<port>/<event>         (system events)
    def cue(port, chan, event, args)
      path = chan ? "/midi:#{port}:#{chan}/#{event}" : "/midi:#{port}/#{event}"
      @internal_cue_handler.call(path, args) if @internal_cue_handler
    end

    def add_supersonic_midi_handlers!
      # Channel-voice events: args = [port, channel, data…].
      {
        "/midi/in/note_on"          => :note_on,
        "/midi/in/note_off"         => :note_off,
        "/midi/in/control_change"   => :control_change,
        "/midi/in/poly_pressure"    => :aftertouch,       # Sonic Pi's name
        "/midi/in/channel_pressure" => :channel_pressure,
        "/midi/in/pitch_bend"       => :pitch_bend,
        "/midi/in/program_change"   => :program_change,
      }.each do |addr, event|
        @midi_comms.add_method(addr) do |args|
          port = args[0]
          chan = args[1]
          cue(port, chan, event, args[2..-1])
        end
      end

      # System events: args = [port, data…], no channel. Cue names match Tau's.
      {
        "/midi/in/start"         => :start,
        "/midi/in/continue"      => :continue,
        "/midi/in/stop"          => :stop,
        "/midi/in/reset"         => :reset,
        "/midi/in/song_position" => :song_position_pointer,
        "/midi/in/song_select"   => :song_select,
        "/midi/in/time_code"     => :time_code_quarter_frame,
        "/midi/in/tune_request"  => :tune_request,
        "/midi/in/sysex"         => :sysex,
      }.each do |addr, event|
        @midi_comms.add_method(addr) do |args|
          cue(args[0], nil, event, args[1..-1])
        end
      end

      # Derived tempo from an external MIDI clock (a SuperSonic bonus over Tau).
      @midi_comms.add_method("/midi/in/clock_bpm") do |args|
        cue(args[0], nil, "clock_bpm", args[1..-1])
      end

      # Device list: /midi/ports[.reply] = nIn [name enabled]* nOut [name enabled]*.
      ["/midi/ports", "/midi/ports.reply"].each do |addr|
        @midi_comms.add_method(addr) do |args|
          ins, outs = parse_ports(args)
          @updated_midi_ins_handler.call(ins) if @updated_midi_ins_handler
          @updated_midi_outs_handler.call(outs) if @updated_midi_outs_handler
        end
      end
    end

    # Extract the two name lists from a /midi/ports payload.
    def parse_ports(args)
      i = 0
      n_in = args[i].to_i; i += 1
      ins = []
      n_in.times { ins << args[i]; i += 2 }      # skip the enabled flag
      n_out = args[i].to_i; i += 1
      outs = []
      n_out.times { outs << args[i]; i += 2 }
      [ins, outs]
    end
  end
end
