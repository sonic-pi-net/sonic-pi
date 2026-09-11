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
require_relative "osc/timetag"

module SonicPi
  # Spider-side MIDI API over SupersonicMidiComms. Outgoing MIDI is scheduled in
  # SuperSonic's deferred-event scheduler via /schedule (timetagged in
  # SuperClock's domain, so it stays locked to scsynth audio); incoming MIDI
  # arrives as /midi/in/* and is re-emitted as the /midi:<port>:<chan>/<event>
  # cues Sonic Pi already uses. The /midi/* OSC surface lives in SuperSonic
  # (MidiControl + the Rust subsystem).
  class MidiAPI

    # Lang MIDI out-path → SuperSonic /midi/* address.
    OUT_MAP = {
      "/note_on"         => "/clockwork/midi/out/note_on",
      "/note_off"        => "/clockwork/midi/out/note_off",
      "/control_change"  => "/clockwork/midi/out/control_change",
      "/aftertouch"      => "/clockwork/midi/out/poly_pressure",   # poly key pressure
      "/channel_pressure"=> "/clockwork/midi/out/channel_pressure",
      "/pitch_bend"      => "/clockwork/midi/out/pitch_bend",
      "/program_change"  => "/clockwork/midi/out/program_change",
      "/raw"             => "/clockwork/midi/out/raw",
      "/sysex"           => "/clockwork/midi/out/sysex",
      "/clock"           => "/clockwork/midi/out/clock",
      "/clock_beat"      => "/clockwork/midi/clock/beat",
      "/start"           => "/clockwork/midi/out/start",
      "/stop"            => "/clockwork/midi/out/stop",
      "/continue"        => "/clockwork/midi/out/continue",
    }.freeze

    # Out-paths whose 2nd arg is the channel (so channel '*' → -1 fans to 1..16).
    CHANNEL_VOICE = %w[
      /note_on /note_off /control_change /aftertouch
      /channel_pressure /pitch_bend /program_change
    ].freeze

    def initialize(supersonic_host, supersonic_port, handlers, disabled_ports = {})
      @midi_comms = SonicPi::SupersonicMidiComms.new(supersonic_host, supersonic_port)
      @internal_cue_handler = handlers[:internal_cue]
      @updated_midi_ins_handler = handlers[:updated_midi_ins]
      @updated_midi_outs_handler = handlers[:updated_midi_outs]
      # Called with {in: [names], out: [names]} whenever the user's
      # disabled-port set changes, so the host can persist it.
      @disabled_ports_changed_handler = handlers[:disabled_midi_ports_changed]
      # The user's persisted per-port mutes. The engine itself is stateless
      # (it forgets per-port enables on restart and on device disconnect),
      # so this is the source of truth, re-asserted on every ports
      # broadcast — which also covers hotplug reconnects.
      @disabled_ports = {
        in:  (disabled_ports[:in]  || []).map(&:to_s),
        out: (disabled_ports[:out] || []).map(&:to_s)
      }
      @global_timewarp = 0

      add_supersonic_midi_handlers!
      @midi_comms.subscribe_to_notifications!
      # sp_midi opened every port; preserve that — open all in + out,
      # then mute the user's disabled ports before any events can cue.
      @midi_comms.send("/clockwork/midi/in/enable", "*", 1)
      @midi_comms.send("/clockwork/midi/out/enable", "*", 1)
      @disabled_ports.each do |dir, names|
        names.each { |n| @midi_comms.send("/clockwork/midi/#{dir}/enable", n, 0) }
      end
      @midi_comms.send("/clockwork/midi/ports/list")   # prime the device lists
    end

    # Schedule outgoing MIDI at spider time `t` (seconds). `midi_path` is the
    # lang-side MIDI path (e.g. "/note_on"), mapped to its SuperSonic address.
    def midi_send_at(t, midi_path, *args)
      ss_addr = OUT_MAP[midi_path]
      return unless ss_addr

      if CHANNEL_VOICE.include?(midi_path) && args[1] == -1
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

    # Cancel pending scheduled MIDI on run stop. The events live in SuperSonic's
    # deferred-event scheduler, so flush there (the same scheduler-level /sched/
    # flush the OSC path uses; tag "default" matches scheduled MIDI + OSC).
    def midi_flush!
      @midi_comms.send("/clockwork/sched/flush", "default")
    end

    def midi_refresh_devices!
      @midi_comms.send("/clockwork/midi/refresh")
    end

    # Mute/unmute a single port. The engine applies it to the live session
    # and rebroadcasts /midi/ports; we own the persistent record.
    def midi_port_enable!(direction, port, enabled)
      dir = direction.to_s == "out" ? :out : :in
      port = port.to_s
      if enabled
        @disabled_ports[dir].delete(port)
      else
        @disabled_ports[dir] << port unless @disabled_ports[dir].include?(port)
      end
      @disabled_ports_changed_handler.call(@disabled_ports) if @disabled_ports_changed_handler
      @midi_comms.send("/clockwork/midi/#{dir}/enable", port, enabled ? 1 : 0)
    end

    def set_global_timewarp!(time)
      @global_timewarp = time.to_f / 1000.0
    end

    private

    def send_one(t, ss_addr, args)
      inner = @midi_comms.encoder.encode_single_message(ss_addr, args)
      tt = SonicPi::OSC.osc_timetag(t + @global_timewarp)
      # /schedule <timetag> <inner /midi/* blob>: the scheduler re-ingests the
      # inner message on time through the same dispatch an immediate one hits.
      @midi_comms.send("/clockwork/schedule",
                       SonicPi::OSC::Int64.new(tt),
                       SonicPi::OSC::Blob.new(inner))
    end

    # Re-emit a /midi/in/* push as a Sonic Pi cue:
    #   /midi:<port>:<chan>/<event>  (channel events)
    #   /midi:<port>/<event>         (system events)
    def cue(port, chan, event, args)
      path = chan ? "/midi:#{port}:#{chan}/#{event}" : "/midi:#{port}/#{event}"
      @internal_cue_handler.call(path, args) if @internal_cue_handler
    end

    def add_supersonic_midi_handlers!
      # Channel-voice events: args = [port, channel, data…].
      {
        "/clockwork/midi/in/note_on"          => :note_on,
        "/clockwork/midi/in/note_off"         => :note_off,
        "/clockwork/midi/in/control_change"   => :control_change,
        "/clockwork/midi/in/poly_pressure"    => :aftertouch,       # Sonic Pi's name
        "/clockwork/midi/in/channel_pressure" => :channel_pressure,
        "/clockwork/midi/in/pitch_bend"       => :pitch_bend,
        "/clockwork/midi/in/program_change"   => :program_change,
      }.each do |addr, event|
        @midi_comms.add_method(addr) do |args|
          port = args[0]
          chan = args[1]
          cue(port, chan, event, args[2..-1])
        end
      end

      # System events: args = [port, data…], no channel.
      {
        "/clockwork/midi/in/start"         => :start,
        "/clockwork/midi/in/continue"      => :continue,
        "/clockwork/midi/in/stop"          => :stop,
        "/clockwork/midi/in/reset"         => :reset,
        "/clockwork/midi/in/song_position" => :song_position_pointer,
        "/clockwork/midi/in/song_select"   => :song_select,
        "/clockwork/midi/in/time_code"     => :time_code_quarter_frame,
        "/clockwork/midi/in/tune_request"  => :tune_request,
        "/clockwork/midi/in/sysex"         => :sysex,
      }.each do |addr, event|
        @midi_comms.add_method(addr) do |args|
          cue(args[0], nil, event, args[1..-1])
        end
      end

      # Derived tempo from an external MIDI clock.
      @midi_comms.add_method("/clockwork/midi/in/clock_bpm") do |args|
        cue(args[0], nil, "clock_bpm", args[1..-1])
      end

      # Device list: /midi/ports[.reply] = nIn [name enabled]* nOut [name enabled]*.
      ["/clockwork/midi/ports", "/clockwork/midi/ports.reply"].each do |addr|
        @midi_comms.add_method(addr) do |args|
          ins, outs = parse_ports(args)
          reassert_disabled_ports!(:in, ins)
          reassert_disabled_ports!(:out, outs)
          @updated_midi_ins_handler.call(ins) if @updated_midi_ins_handler
          @updated_midi_outs_handler.call(outs) if @updated_midi_outs_handler
        end
      end
    end

    # The engine forgets a port's enable state when it disconnects, so a
    # persistently-muted device that reappears comes back enabled — re-mute
    # it as soon as a broadcast reports it enabled. The engine only
    # rebroadcasts on actual state change, so this cannot loop.
    def reassert_disabled_ports!(dir, pairs)
      pairs.each do |name, enabled|
        if enabled == 1 && @disabled_ports[dir].include?(name)
          @midi_comms.send("/clockwork/midi/#{dir}/enable", name, 0)
        end
      end
    end

    # Extract the two [name, enabled] pair lists from a /midi/ports payload.
    def parse_ports(args)
      i = 0
      n_in = args[i].to_i; i += 1
      ins = []
      n_in.times { ins << [args[i], args[i + 1].to_i]; i += 2 }
      n_out = args[i].to_i; i += 1
      outs = []
      n_out.times { outs << [args[i], args[i + 1].to_i]; i += 2 }
      [ins, outs]
    end
  end
end
