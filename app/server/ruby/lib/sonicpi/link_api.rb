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
require_relative "supersonic_link_comms"

module SonicPi
  # Spider-side Ableton Link API over SupersonicLinkComms: tempo,
  # beat/clock conversions, transport, peer count, Link Audio I/O.
  # The /clock/* OSC surface lives in SuperSonic's
  # EngineControl.cpp::handleLinkCommand (native control) and
  # EngineClock.cpp::handleClockCoreOsc (cross-platform clock core).
  class LinkAPI

    # Seconds between the NTP (1900) and Unix (1970) epochs. The engine answers
    # time queries in NTP seconds (SuperClock's wall-clock domain); this constant
    # is the whole conversion to Ruby's Unix time. It never changes and is never
    # measured, so there's nothing to drift or go stale across a sleep/wake.
    NTP_EPOCH_OFFSET = 2_208_988_800

    def initialize(supersonic_host, supersonic_port, handlers)
      @incoming_tempo_change_cv = ConditionVariable.new
      @incoming_tempo_change_mut = Mutex.new

      @link_comms = SonicPi::SupersonicLinkComms.new(supersonic_host, supersonic_port)
      @internal_cue_handler = handlers[:internal_cue]
      @updated_link_num_peers_handler = handlers[:updated_link_num_peers]
      @updated_link_bpm_handler = handlers[:updated_link_bpm]

      # Per-timeline tempo cache (name => bpm), refreshed by the /clock/notify
      # pushes so reads are cheap + stable. "link" is just another key.
      @timeline_tempos = {}

      add_supersonic_link_handlers!
    end

    # Subscribe to /clock/notify/* and enable Link start/stop sync. Must run
    # once SuperSonic is up: subscribing from the constructor races the engine's
    # UDP bind and the datagram is silently dropped.
    def link_system_start!
      @link_comms.subscribe_to_notifications!
      # Peer play/stop drives the /link/start and /link/stop cues link_sync waits on.
      link_set_start_stop_sync_enabled!(true)
    end

    def link_is_on?
      res = @link_comms.rpc("/clock/enabled/get",
                            expect: "/clock/enabled.reply")
      res ? (res[0].to_i != 0) : false
    end

    def link_disable
      @link_comms.send("/clock/visibility", 0)
    end

    def link_enable
      # 2 = NetworkWide; use /clock/visibility 1 for loopback-only.
      @link_comms.send("/clock/visibility", 2)
    end

    def link_reset
      @link_comms.send("/clock/reset")
    end

    # Visibility shortcut: 0 = Off, 1 = LoopbackOnly, 2 = NetworkWide.
    def link_set_visibility!(mode)
      @link_comms.send("/clock/visibility", mode.to_i)
    end

    def link_get_visibility
      res = @link_comms.rpc("/clock/visibility/get",
                            expect: "/clock/visibility.reply")
      res ? res[0].to_i : 0
    end

    # Link Audio publish gate; defaults off in SuperSonic, opt-in. While
    # off, channels aren't advertised even with the Link mesh up.
    def link_audio_publish_set!(enabled)
      @link_comms.send("/clock/audio/publish/set", enabled ? 1 : 0)
    end

    def link_audio_publish_get
      res = @link_comms.rpc("/clock/audio/publish/get",
                            expect: "/clock/audio/publish.reply")
      res ? (res[0].to_i != 0) : false
    end

    # Peer name advertised to other Link apps (Sonic Pi sets it on boot).
    def link_peer_name_set!(name)
      @link_comms.send("/clock/peer_name/set", name.to_s)
    end

    def link_peer_name_get
      res = @link_comms.rpc("/clock/peer_name/get",
                            expect: "/clock/peer_name.reply")
      res ? res[0].to_s : ""
    end

    # Subscribe to a remote (peer, channel) Link Audio stream, rendered
    # stereo into bus and bus+1. Idempotent per (peer, channel): re-issuing
    # remaps the bus. Subscriptions are concurrent, each into its own pair.
    def link_audio_input_set!(peer, channel, bus)
      @link_comms.send("/clock/audio/input/add",
                       peer.to_s, channel.to_s, bus.to_i)
    end

    def link_audio_input_remove!(peer, channel)
      @link_comms.send("/clock/audio/input/remove",
                       peer.to_s, channel.to_s)
    end

    def link_audio_inputs_clear!
      @link_comms.send("/clock/audio/input/clear")
    end

    # Per-input receive latency in seconds (0-2 s typical). The GUI's
    # latency slider sets this for every active input.
    def link_audio_input_latency_set!(peer, channel, seconds)
      @link_comms.send("/clock/audio/input/latency/set",
                       peer.to_s, channel.to_s, seconds.to_f)
    end

    def link_get_start_stop_sync_enabled
      res = @link_comms.rpc("/clock/start_stop_sync/get",
                            expect: "/clock/start_stop_sync.reply")
      res ? (res[0].to_i != 0) : false
    end

    def link_set_start_stop_sync_enabled!(enabled)
      @link_comms.send("/clock/start_stop_sync/set", enabled ? 1 : 0)
    end

    def link_num_peers
      res = @link_comms.rpc("/clock/peers/count/get",
                            expect: "/clock/peers/count.reply")
      res ? res[0].to_i : 0
    end

    # Build a /clock/<tl>/<verb> address. tl == "link" omits the segment for
    # wire back-compat (the engine treats omitted == link). <verb> may be a
    # request ("tempo/get") or a reply suffix ("tempo.reply").
    def clock_addr(verb, tl)
      tl == "link" ? "/clock/#{verb}" : "/clock/#{tl}/#{verb}"
    end

    # tl: "link" (default), "midi" (engine resolves to the primary midi
    # timeline), or "midi:<handle>" for a specific port. Every timeline's tempo
    # is cached in @timeline_tempos (kept fresh by the /clock/notify pushes); on
    # a miss we read live and, on RPC failure, keep the prior value over 60.0.
    def link_tempo(force_api_call=false, tl: "link")
      cached = @timeline_tempos[tl]
      return cached if cached && !force_api_call
      res = @link_comms.rpc(clock_addr("tempo/get", tl),
                            expect: clock_addr("tempo.reply", tl))
      @timeline_tempos[tl] = res ? res[0].to_f : (cached || 60.0)
    end

    def link_set_bpm!(bpm)
      @link_comms.send("/clock/tempo/set", bpm.to_f)
      # Wait up to 100ms for the notify-push.
      @incoming_tempo_change_mut.synchronize do
        @incoming_tempo_change_cv.wait(@incoming_tempo_change_mut, 0.1)
      end
    end

    def link_get_beat_at_time(time, quantum = 4, tl: "link")
      res = @link_comms.rpc(clock_addr("rpc/beat_at_time", tl),
                            SonicPi::OSC::Int64.new(time), quantum.to_f,
                            expect: clock_addr("rpc/beat_at_time.reply", tl))
      res ? res[0].to_f : 0.0
    end

    def link_get_phase_at_time(time, quantum = 4, tl: "link")
      res = @link_comms.rpc(clock_addr("rpc/phase_at_time", tl),
                            SonicPi::OSC::Int64.new(time), quantum.to_f,
                            expect: clock_addr("rpc/phase_at_time.reply", tl))
      res ? res[0].to_f : 0.0
    end

    def link_get_time_at_beat(beat, quantum = 4, tl: "link")
      res = @link_comms.rpc(clock_addr("rpc/time_at_beat", tl),
                            beat.to_f, quantum.to_f,
                            expect: clock_addr("rpc/time_at_beat.reply", tl))
      res ? res[0].to_i : 0
    end

    # Engine "now" (NTP micros) plus beat and phase at that instant, in a
    # single round-trip (was three serial RPCs: time/now + beat + phase).
    def link_get_now_beat_and_phase(quantum = 4, tl: "link")
      res = @link_comms.rpc(clock_addr("rpc/beat_phase_now", tl),
                            quantum.to_f,
                            expect: clock_addr("rpc/beat_phase_now.reply", tl))
      res ? [res[0].to_i, res[1].to_f, res[2].to_f] : [0, 0.0, 0.0]
    end

    def link_get_next_beat_and_time_at_phase(phase, quantum, safety_t, tl: "link")
      safety_micros = safety_t * 1_000_000.0
      fq = quantum.to_f
      t_now, t_now_beat, t_now_phase = link_get_now_beat_and_phase(fq, tl: tl)

      next_whole_quantum = (t_now_beat - t_now_phase) + quantum
      next_beat = next_whole_quantum + phase
      next_time = link_get_time_at_beat(next_beat, fq, tl: tl)

      if (next_time - t_now) < safety_micros
        next_beat += quantum
        next_time = link_get_time_at_beat(next_beat, fq, tl: tl)
      end
      [next_beat, next_time]
    end

    def link_get_next_beat_and_clock_time_at_phase(phase, quantum, safety_t, tl: "link")
      beat, link_time = link_get_next_beat_and_time_at_phase(phase, quantum, safety_t, tl: tl)
      [beat, link_micros_to_clock_time(link_time)]
    end

    def link_get_clock_time_at_beat(beat, quantum = 4, tl: "link")
      link_micros_to_clock_time(link_get_time_at_beat(beat, quantum, tl: tl))
    end

    def link_get_beat_at_clock_time(clock_time, quantum = 4, tl: "link")
      link_get_beat_at_time(clock_time_to_link_micros(clock_time), quantum, tl: tl)
    end

    def link_get_phase_at_clock_time(clock_time, quantum = 4, tl: "link")
      link_get_phase_at_time(clock_time_to_link_micros(clock_time), quantum, tl: tl)
    end

    def link_get_beat_and_phase_at_clock_time(clock_time, quantum = 4, tl: "link")
      res = @link_comms.rpc(clock_addr("rpc/beat_phase_at_time", tl),
                            SonicPi::OSC::Int64.new(clock_time_to_link_micros(clock_time)),
                            quantum.to_f,
                            expect: clock_addr("rpc/beat_phase_at_time.reply", tl))
      res ? [res[0].to_f, res[1].to_f] : [0.0, 0.0]
    end

    def link_set_is_playing!(enabled)
      @link_comms.send("/clock/transport/set", enabled ? 1 : 0)
    end

    def link_is_playing?(tl: "link")
      link_transport_state(tl: tl)[:playing]
    end

    # Transport state for any timeline. anchored = a transport event (START or
    # SPP) has defined the timeline's beat origin — always true for link, whose
    # session grid exists independent of transport. midi_sync gates on both
    # flags. An unclaimed timeline (no clock seen yet) reports false/false.
    def link_transport_state(tl: "link")
      res = @link_comms.rpc(clock_addr("transport/get", tl),
                            expect: clock_addr("transport.reply", tl))
      { playing:  res ? (res[0].to_i != 0) : false,
        anchored: res ? (res[1].to_i != 0) : false }
    end

    # Enumerate all timelines the engine knows: the Link timeline plus any
    # active midi:<port> follower. One hash per row; `name` is the wire id
    # ("link" / "midi:<handle>"), `raw` the friendly OS device name.
    def clock_timelines
      res = @link_comms.rpc("/clock/timelines/get",
                            expect: "/clock/timelines.reply")
      return [] unless res
      res.each_slice(6).map do |name, raw, bpm, clocking, stale, primary|
        { name: name.to_s, raw: raw.to_s, bpm: bpm.to_f,
          clocking: clocking.to_i != 0, stale: stale.to_i != 0,
          primary: primary.to_i != 0 }
      end
    end

    def link_get_time_for_is_playing
      res = @link_comms.rpc("/clock/transport/time/get",
                            expect: "/clock/transport/time.reply")
      res ? res[0].to_i : 0
    end

    def link_current_time
      res = @link_comms.rpc("/clock/time/now/get",
                            expect: "/clock/time/now.reply")
      res ? res[0].to_i : 0
    end

    def link_current_time_and_beat(quantise_beat=true)
      link_time, beat, _phase = link_get_now_beat_and_phase

      if quantise_beat
        beat = (beat + 1).to_i
        link_time = link_get_time_at_beat(beat)
      end

      [link_micros_to_clock_time(link_time), beat]
    end

    def link_sleep(s)
      t1 = Time.now
      @incoming_tempo_change_mut.synchronize do
        @incoming_tempo_change_cv.wait(@incoming_tempo_change_mut, s)
      end
      t2 = Time.now
      if (t2 - t1) < (s + 0.05)
        yield
      end
    end

    private

    # The engine answers time queries in the NTP (wall-clock) domain, so the only
    # conversion to Ruby's Unix time is the fixed NTP<->Unix epoch constant — no
    # measured, drift-prone offset. t == 0 means the RPC upstream failed/timed
    # out; fall back to wall-clock now rather than converting a bogus 0 (which is
    # epoch 1900), logged rate-limited to once a second.
    def link_micros_to_clock_time(t)
      if t == 0
        now = Process.clock_gettime(Process::CLOCK_MONOTONIC)
        if now - (@last_zero_lookup_log || 0) >= 1.0
          @last_zero_lookup_log = now
          STDOUT.puts "Spider - LINK time lookup failed (0) — using wall clock"
          STDOUT.flush
        end
        return Process.clock_gettime(Process::CLOCK_REALTIME)
      end
      t / 1_000_000.0 - NTP_EPOCH_OFFSET
    end

    def clock_time_to_link_micros(t)
      ((t + NTP_EPOCH_OFFSET) * 1_000_000).round
    end

    def add_supersonic_link_handlers!
      # Session tempo changed (locally or by a peer).
      @link_comms.add_method("/clock/notify/tempo") do |args|
        tempo = args[0].to_f
        @timeline_tempos["link"] = tempo
        @updated_link_bpm_handler.call(tempo) if @updated_link_bpm_handler
        @incoming_tempo_change_cv.broadcast
        @internal_cue_handler.call("/link/tempo-change", [tempo]) if @internal_cue_handler
      end

      # Visible peer count changed.
      @link_comms.add_method("/clock/notify/peers") do |args|
        n = args[0].to_i
        @updated_link_num_peers_handler.call(n) if @updated_link_num_peers_handler
        @internal_cue_handler.call("/link/num-peers", [n]) if @internal_cue_handler
        # connected/disconnected cues on 0<->non-0 transitions.
        prev = @prev_link_num_peers || 0
        if prev == 0 && n > 0
          @internal_cue_handler.call("/link/connected", []) if @internal_cue_handler
        elsif prev > 0 && n == 0
          @internal_cue_handler.call("/link/disconnected", []) if @internal_cue_handler
        end
        @prev_link_num_peers = n
      end

      # A timeline set/tempo changed (add/remove/stale/primary or a midi tempo
      # crossing the engine's notify threshold). Args repeat per timeline:
      # name(s) raw(s) bpm(f) clocking(i) stale(i) primary(i). Refresh the
      # per-timeline tempo cache and wake clock-mode sleeps so midi riders
      # re-anchor immediately — the midi analog of /clock/notify/tempo.
      @link_comms.add_method("/clock/timelines") do |args|
        args.each_slice(6) do |name, _raw, bpm, _clocking, _stale, _primary|
          @timeline_tempos[name.to_s] = bpm.to_f if name
        end
        @incoming_tempo_change_cv.broadcast
        @internal_cue_handler.call("/midi/clock-change", []) if @internal_cue_handler
      end

      # Transport state changed. Args: <int> playing, <int64> at-NTP-micros
      # (the engine converts out of the Link clock domain before broadcast).
      # The cue carries the transition's clock time so waiters can align to
      # the actual transport edge rather than cue-delivery time.
      @link_comms.add_method("/clock/notify/transport") do |args|
        playing = args[0].to_i != 0
        cue = playing ? "/link/start" : "/link/stop"
        t = args[1] ? link_micros_to_clock_time(args[1].to_i) : nil
        @internal_cue_handler.call(cue, t ? [t] : []) if @internal_cue_handler
      end
    end
  end
end
