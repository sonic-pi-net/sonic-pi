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
  class TauAPI
    attr_reader :booted

    def initialize(ports, handlers)
      @incoming_tempo_change_cv = ConditionVariable.new
      @incoming_tempo_change_mut = Mutex.new
      @tau_api_events = IncomingEvents.new
      @client_id = @tau_api_events.gensym("RubyTauAPI")
      @osc_cues_port = ports[:osc_cues_port]
      @tau_port = ports[:tau_port]
      @tau_host = "127.0.0.1"
      @listen_to_tau_port = ports[:listen_to_tau_port]
      @global_timewarp = 0

      @tau_comms = SonicPi::TauComms.new("127.0.0.1", @tau_port, @listen_to_tau_port)
      @external_osc_cue_handler = handlers[:external_osc_cue]
      @internal_cue_handler = handlers[:internal_cue]
      @updated_midi_ins_handler = handlers[:updated_midi_ins]
      @updated_midi_outs_handler = handlers[:updated_midi_outs]
      @updated_link_num_peers_handler = handlers[:updated_link_num_peers]
      @updated_link_bpm_handler = handlers[:updated_link_bpm]

      add_incoming_api_handlers!

      @tempo = nil
      @link_time_micros = nil
      @local_time_micros = nil
      @link_time_delta_micros = 0

      block_until_tau_ready!

      Thread.new do
        # Continually update link time delta every 5 seconds this is
        # particularly necessary in the case where Sonic Pi is running
        # on a machine that can be 'put to sleep' such as a laptop - in
        # which case this delta always needs updating after being woken
        # to account for the amount of time the laptop was closed for
        # and the OS was in a hibernation state.
        loop do
          update_link_time_delta!
          Kernel.sleep 5
        end
      end
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

    def send_midi_at(t, path, *args)
      b = OSC::Blob.new(@tau_comms.encoder.encode_single_message(path, args))
      api_send_at(t + @global_timewarp, "/midi-at", b)
    end

    # Link API

    def link_is_on?
      res = api_rpc("/link-is-on")
      res[0]
    end

    def link_disable
      @tau_comms.send("/link-disable")
    end

    def link_enable
      @tau_comms.send("/link-enable")
    end

    def link_reset
      @tau_comms.send("/link-reset")
    end

    def link_get_start_stop_sync_enabled
      res = api_rpc("/link-get-start-stop-sync-enabled")
      res[0]
    end

    def link_set_start_stop_sync_enabled!(enabled)
      @tau_comms.send("/link-set-start-stop-sync-enabled", !!enabled)
    end

    def link_num_peers
      res = api_rpc("/link-get-num-peers")
      res[0].to_i
    end

    def link_tempo(force_api_call=false)
      if force_api_call
        res = api_rpc("/link-get-tempo")
        @tempo = res[0]
        return res[0]
      elsif @tempo
        return @tempo
      else
        @tempo = link_tempo(true)
      end
    end

    def link_set_bpm_at_clock_time!(bpm, clock_time)
      res = @tau_comms.send_ts(clock_time, "/link-set-tempo", bpm.to_f)

      # Wait for a max of 100ms for the next tempo change to come in...
      @incoming_tempo_change_mut.synchronize do
        @incoming_tempo_change_cv.wait(@incoming_tempo_change_mut, 0.1)
      end
      res
    end

    def link_get_beat_at_time(time, quantum = 4)
      res = api_rpc("/link-get-beat-at-time", SonicPi::OSC::Int64.new(time), quantum)
      res[0]
    end

    def link_get_phase_at_time(time, quantum = 4)
      res = api_rpc("/link-get-phase-at-time", SonicPi::OSC::Int64.new(time), quantum)
      res[0]
    end

    def link_get_time_at_beat(beat, quantum = 4)
      res = api_rpc("/link-get-time-at-beat", beat, quantum)
      res[0]
    end

    def link_get_next_beat_and_time_at_phase(phase, quantum, safety_t)
      res = api_rpc("/link-get-next-beat-and-time-at-phase", phase, quantum, safety_t)
      res
    end

    def link_get_next_beat_and_clock_time_at_phase(phase, quantum, safety_t)
      beat, link_time = link_get_next_beat_and_time_at_phase(phase, quantum, safety_t)
      t_with_delta = (link_time + @link_time_delta_micros) / 1_000_000.0
      [beat, t_with_delta]
    end

    def link_get_clock_time_at_beat(beat, quantum = 4)
      link_time = link_get_time_at_beat(beat, quantum)
      t_with_delta = (link_time + @link_time_delta_micros) / 1_000_000.0
      t_with_delta
    end

    def link_get_beat_at_clock_time(clock_time, quantum = 4)
      link_time = (clock_time * 1_000_000) - @link_time_delta_micros
      link_get_beat_at_time(link_time)
    end

    def link_get_phase_at_clock_time(clock_time, quantum = 4)
      link_time = (clock_time * 1_000_000) - @link_time_delta_micros
      link_get_phase_at_time(link_time)
    end

    def link_get_phase_and_beat_at_clock_time(clock_time, quantum = 4)
      link_time = (clock_time * 1_000_000) - @link_time_delta_micros
      api_rpc("/link-get-phase-and-beat-at-time", SonicPi::OSC::Int64.new(link_time), quantum)
    end

    def link_set_is_playing!(enabled, clock_time)
      @tau_comms.send_ts(clock_time, "/link-set-is-playing", !!enabled)
    end

    def link_is_playing?
      res = api_rpc("/link-get-is-playing")
      res[0]
    end

    def link_get_time_for_is_playing
      res = api_rpc("/link-get-time-for-is-playing")
      res[0]
    end

    def link_current_time
      res = api_rpc("/link-get-current-time")
      res[0].to_i
    end

    def link_current_time_and_beat(quantise_beat=true)
      link_time = link_current_time
      beat = link_get_beat_at_time(link_time)

      if quantise_beat
        beat = (beat + 1).to_i
        link_time = link_get_time_at_beat(beat)
      end

      clock_time = (link_time + @link_time_delta_micros) / 1_000_000.0

      [clock_time, beat]
    end

    def midi_system_start!
      @tau_comms.send("/stop-start-midi-cues", 1)
    end

    def midi_system_stop!
      @tau_comms.send("/stop-start-midi-cues", 0)
    end

    def start_stop_cue_server!(stop)
      @tau_comms.send("/stop-start-cue-server", !stop)
    end

    def cue_server_internal!(internal)
      @tau_comms.send("/osc-in-udp-loopback-restricted", !!internal)
    end

    def midi_flush!
      @tau_comms.send("/midi-flush")
    end

    def osc_flush!
      @tau_comms.send("/flush", "default")
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

    def set_global_timewarp!(time)
      @global_timewarp = time.to_f / 1000.0
    end

    private

    def update_link_time_delta!
      @link_time_micros = link_current_time
      @clock_time_micros = Time.now.to_r * 1_000_000
      delta_micros = @clock_time_micros - @link_time_micros
      @link_time_delta_micros =  delta_micros
    end

    def add_incoming_api_handlers!
      @tau_comms.add_method("/link-tempo-change") do |args|
        _gui_id = args[0]
        tempo = args[1].to_f
        @updated_link_bpm_handler.call(tempo)
        @incoming_tempo_change_cv.broadcast
        @tempo = tempo
      end

      @tau_comms.add_method("/link-num-peers") do |args|
        _gui_id = args[0]
        num_peers = args[1].to_i
        @updated_link_num_peers_handler.call(num_peers)
      end

      @tau_comms.add_method("/midi-ins") do |args|
        _gui_id = args[0]
        ins = args[1..-1]
        @updated_midi_ins_handler.call(ins)
      end

      @tau_comms.add_method("/midi-outs") do |args|
        _gui_id = args[0]
        outs = args[1..-1]
        @updated_midi_outs_handler.call(outs)
      end

      @tau_comms.add_method("/tau-api-reply") do |args|
        _gui_id = args[0]
        key = args[1]
        payload = args[2..-1]
        @tau_api_events.async_event(key, payload)
      end

      @tau_comms.add_method("/internal-cue") do |args|
        _gui_id = args[0]
        path = args[1]
        args = args[2..-1]
        @internal_cue_handler.call(path, args)
      end

      @tau_comms.add_global_method do |e, args|
        log "got incoming #{e}, #{args}"
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
