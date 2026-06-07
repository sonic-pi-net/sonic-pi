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

    def initialize(supersonic_host, supersonic_port, handlers)
      @incoming_tempo_change_cv = ConditionVariable.new
      @incoming_tempo_change_mut = Mutex.new

      @link_comms = SonicPi::SupersonicLinkComms.new(supersonic_host, supersonic_port)
      @internal_cue_handler = handlers[:internal_cue]
      @updated_link_num_peers_handler = handlers[:updated_link_num_peers]
      @updated_link_bpm_handler = handlers[:updated_link_bpm]

      @tempo = nil
      @link_time_delta_micros = 0

      add_supersonic_link_handlers!

      Thread.new do
        # Refresh the wall-clock/Link-clock offset; mainly to recover
        # after a laptop sleep/wake.
        loop do
          update_link_time_delta!
          Kernel.sleep 5
        end
      end
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

    def link_tempo(force_api_call=false)
      return @tempo if @tempo && !force_api_call
      res = @link_comms.rpc("/clock/tempo/get",
                            expect: "/clock/tempo.reply")
      # On RPC failure keep the prior @tempo rather than caching 60.0.
      @tempo = res ? res[0].to_f : (@tempo || 60.0)
    end

    def link_set_bpm!(bpm)
      @link_comms.send("/clock/tempo/set", bpm.to_f)
      # Wait up to 100ms for the notify-push.
      @incoming_tempo_change_mut.synchronize do
        @incoming_tempo_change_cv.wait(@incoming_tempo_change_mut, 0.1)
      end
    end

    def link_get_beat_at_time(time, quantum = 4)
      res = @link_comms.rpc("/clock/rpc/beat_at_time",
                            SonicPi::OSC::Int64.new(time), quantum.to_f,
                            expect: "/clock/rpc/beat_at_time.reply")
      res ? res[0].to_f : 0.0
    end

    def link_get_phase_at_time(time, quantum = 4)
      res = @link_comms.rpc("/clock/rpc/phase_at_time",
                            SonicPi::OSC::Int64.new(time), quantum.to_f,
                            expect: "/clock/rpc/phase_at_time.reply")
      res ? res[0].to_f : 0.0
    end

    def link_get_time_at_beat(beat, quantum = 4)
      res = @link_comms.rpc("/clock/rpc/time_at_beat",
                            beat.to_f, quantum.to_f,
                            expect: "/clock/rpc/time_at_beat.reply")
      res ? res[0].to_i : 0
    end

    def link_get_next_beat_and_time_at_phase(phase, quantum, safety_t)
      safety_micros = safety_t * 1_000_000.0
      fq = quantum.to_f
      t_now = link_current_time
      t_now_phase = link_get_phase_at_time(t_now, fq)
      t_now_beat = link_get_beat_at_time(t_now, fq)

      next_whole_quantum = (t_now_beat - t_now_phase) + quantum
      next_beat = next_whole_quantum + phase
      next_time = link_get_time_at_beat(next_beat, fq)

      if (next_time - t_now) < safety_micros
        next_beat += quantum
        next_time = link_get_time_at_beat(next_beat, fq)
      end
      [next_beat, next_time]
    end

    def link_get_next_beat_and_clock_time_at_phase(phase, quantum, safety_t)
      beat, link_time = link_get_next_beat_and_time_at_phase(phase, quantum, safety_t)
      [beat, link_micros_to_clock_time(link_time)]
    end

    def link_get_clock_time_at_beat(beat, quantum = 4)
      link_micros_to_clock_time(link_get_time_at_beat(beat, quantum))
    end

    def link_get_beat_at_clock_time(clock_time, quantum = 4)
      link_get_beat_at_time(clock_time_to_link_micros(clock_time))
    end

    def link_get_phase_at_clock_time(clock_time, quantum = 4)
      link_get_phase_at_time(clock_time_to_link_micros(clock_time))
    end

    def link_get_phase_and_beat_at_clock_time(clock_time, quantum = 4)
      # No combined phase+beat RPC, so two calls.
      link_time = clock_time_to_link_micros(clock_time)
      [link_get_phase_at_time(link_time, quantum),
       link_get_beat_at_time(link_time, quantum)]
    end

    def link_set_is_playing!(enabled)
      @link_comms.send("/clock/transport/set", enabled ? 1 : 0)
    end

    def link_is_playing?
      res = @link_comms.rpc("/clock/transport/get",
                            expect: "/clock/transport.reply")
      res ? (res[0].to_i != 0) : false
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
      link_time = link_current_time
      beat = link_get_beat_at_time(link_time)

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

    def link_micros_to_clock_time(t)
      (t + @link_time_delta_micros) / 1_000_000.0
    end

    def clock_time_to_link_micros(t)
      (t * 1_000_000) - @link_time_delta_micros
    end

    def update_link_time_delta!
      link_micros = link_current_time
      # Skip on RPC failure (0); else delta becomes wall-clock-now and
      # corrupts conversions until the next good refresh.
      return if link_micros == 0
      clock_micros = (Process.clock_gettime(Process::CLOCK_REALTIME, :microsecond))
      @link_time_delta_micros = clock_micros - link_micros
    end

    def add_supersonic_link_handlers!
      # Session tempo changed (locally or by a peer).
      @link_comms.add_method("/clock/notify/tempo") do |args|
        tempo = args[0].to_f
        @tempo = tempo
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

      # Transport state changed. Args: <int> playing, <int64> at-link-micros.
      @link_comms.add_method("/clock/notify/transport") do |args|
        playing = args[0].to_i != 0
        cue = playing ? "/link/start" : "/link/stop"
        @internal_cue_handler.call(cue, []) if @internal_cue_handler
      end

      @link_comms.subscribe_to_notifications!

      # Peer play/stop drives the /link/start and /link/stop cues link_sync waits on.
      link_set_start_stop_sync_enabled!(true)

      # Sonic Pi defaults to 60 BPM; SuperSonic's Link defaults to 120, so
      # push ours on boot. Joining a Link session overrides it as normal.
      @link_comms.send("/clock/tempo/set", 60.0)
    end
  end
end
