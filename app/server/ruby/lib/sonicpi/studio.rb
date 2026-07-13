# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative "util"
require_relative "server"
require_relative "note"
require_relative "samplebuffer"
require_relative "studio_ready_gate"

require 'set'
require 'fileutils'

module SonicPi
  class Studio

    # StudioCurrentlyRebootingError now lives in studio_ready_gate.rb
    # so the gate primitive can raise it. Aliased here so any external
    # rescue clauses written as `rescue Studio::StudioCurrentlyRebootingError`
    # keep working.
    StudioCurrentlyRebootingError = ::SonicPi::StudioCurrentlyRebootingError
    include Util

    attr_reader :synth_group, :fx_group, :mixer_group, :monitor_group, :mixer_id, :mixer_bus, :mixer, :rand_buf_id, :amp, :rebooting, :last_cold_swap_completed_at

    attr_accessor :cent_tuning

    def initialize(ports, msg_queue, state, register_cue_event_lambda, current_spider_time_lambda)

      STDOUT.puts "studio - init"
      STDOUT.flush

      @state = state
      @scsynth_port = ports[:scsynth_port]
      @scsynth_send_port = ports[:scsynth_send_port]
      @msg_queue = msg_queue
      @error_occured_mutex = Mutex.new
      @error_occurred_since_last_check = false
      @sample_sem = Mutex.new
      @reboot_mutex = Mutex.new
      @rebooting = false
      # Wall-clock timestamp of the most-recent successful
      # cold_swap_reinit. Read by lang/core.rb's sleep to grant a
      # grace window for "thread too far behind time" errors caused
      # by the cold-swap pause itself (the gate blocks all trigger
      # threads for the duration of the reinit, which the timing
      # safety check would otherwise treat as the thread running
      # behind and kill the live_loop).
      @last_cold_swap_completed_at = nil
      # [peer, channel] tuple => AudioBus subscribed via Link Audio. Each
      # tuple is a separate stream, kept across :stop / re-trigger so the
      # same identity always lands on the same bus. Cleared on reboot /
      # cold-swap by reset_and_setup_groups_and_busses (@server.reset!
      # wipes the bus allocator, so these references would dangle).
      @link_audio_subs = {}
      @link_audio_mut  = Mutex.new
      # Reader-writer gate. Studio-touching methods (trigger_synth,
      # new_group, allocate_buffer, etc.) hold the read lock for the
      # duration of their work; cold_swap_reinit holds the write lock
      # around its phases, draining readers first. Guarantees user
      # code that's mid-trigger can't see a partially-nilled studio.
      # Reentrant per-thread so trigger_fx → trigger_synth doesn't
      # self-deadlock and cold_swap_reinit's own phases can call
      # studio methods.
      @studio_ready_gate = StudioReadyGate.new
      # Stays true across all six phases of cold_swap_reinit. @rebooting
      # is cleared after Phase 1 so the Studio's own methods (called by
      # Phases 2-6) can run; this separate flag is what user-eval
      # threads block on at __spider_eval entry — without it they would
      # touch mid-rebuild refs (mixer_group becomes nil mid-Phase-2) and
      # crash with NoMethodError on ChainNode#initialize.
      @cold_swap_reinit_in_progress = false
      @reboot_done_cv = ConditionVariable.new
      @reboot_done_mutex = Mutex.new
      @cent_tuning = 0
      @sample_format = "int16"
      @paused = false
      @register_cue_event_lambda = register_cue_event_lambda
      @current_spider_time_lambda = current_spider_time_lambda
      @global_timewarp = 0
      init_scsynth
      reset_server
      init_studio
    end

    def __exec_path(path)
      case os
      when :windows
        path
      else
        "exec #{path}"
      end
    end

    def stop_midi(silent=false)
      message "MIDI Subsystems stopped." unless silent
    end

    def start_midi(silent=false)
      message "MIDI Subsystems started." unless silent
    end

    def init_scsynth
      @server = Server.new(@scsynth_port, @msg_queue, @state, @register_cue_event_lambda, @current_spider_time_lambda)
      message "Initialised SuperSonic #{@server.version}"
    end

    def init_studio
      @server.load_synthdefs(Paths.synthdef_path)
      @amp = [0.0, 1.0]
      @server.add_event_handler("/sonic-pi/amp", "/sonic-pi/amp") do |payload|
        @amp = [payload[2], payload[3]]
      end

      old_synthdefs = @loaded_synthdefs
      @loaded_synthdefs = Set.new

      (old_synthdefs || []).each do |s|
        message "Reloading synthdefs in #{unify_tilde_dir(s)}"
        internal_load_synthdefs(s, @server)
      end

      # load rand stream directly - ensuring it doesn't get considered as a 'sample'
      rand_buf = @server.buffer_alloc_read(Paths.buffers_path + "/rand-stream.wav")

      @sample_sem.synchronize do
        @buffers = {}
      end

      old_samples = @samples
      @samples = {}

      Thread.new do
        __system_thread_locals.set_local(:sonic_pi_local_thread_group, "Studio sample loader")
        Thread.current.priority = -10
        (old_samples || {}).each do |k, v|
          message "Reloading sample - #{unify_tilde_dir(k)}"
          internal_load_sample(k, @server)
        end
      end

      @recorders = {}
      @recording_mutex = Mutex.new

      rand_buf.wait_for_allocation
      @rand_buf_id = rand_buf.to_i

      @control_bus_mutex = Mutex.new
      @control_busses = {}
    end

    def error_occurred?
      @error_occured_mutex.synchronize do
        if @error_occurred_since_last_check
          @error_occurred_since_last_check = false
          return true
        else
          return false
        end
      end
    end

    def scsynth_info
      @server.scsynth_info
    end

    def allocate_buffer(name, duration_in_seconds)
      check_for_server_rebooting!(:allocate_buffer)
      name = name.to_sym
      cached_buffer = @buffers[name]
      return [cached_buffer, true] if cached_buffer && (!duration_in_seconds || (cached_buffer.duration == duration_in_seconds))

      # we can't just return a cached buffer - so grab the semaphore and
      # let's play...
      @sample_sem.synchronize do
        cached_buffer = @buffers[name]
        return [cached_buffer, true] if cached_buffer && (!duration_in_seconds || (cached_buffer.duration == duration_in_seconds))

        # our buffer has the same name but is of a different duration
        # therefore nuke it
        path = Paths.cached_samples_path + "/#{name}.wav"
        # now actually allocate a new buffer and cache it
        sample_rate = @server.scsynth_info[:sample_rate]
        buffer_info = @server.buffer_alloc(duration_in_seconds * sample_rate, 2)
        buffer_info.wait_for_allocation
        buffer_info.path = path
        save_buffer!(buffer_info, path)
        @buffers[name] = buffer_info
        @server.buffer_free(cached_buffer) if cached_buffer
        return [buffer_info, false]
      end
    end

    def free_buffer(name)
      check_for_server_rebooting!(:free_buffer)
      name = name.to_sym

      if @buffers[name]
        @sample_sem.synchronize do
          if @buffers[name]
            @server.buffer_free(@buffers[name])
            @buffers.delete(name)
          end
        end
        return true
      end

      false
    end

    def load_synthdefs(path, server=@server)
      check_for_server_rebooting!(:load_synthdefs)
      internal_load_synthdefs(path, server)
    end

    def load_synthdef(path, server=@server)
      check_for_server_rebooting!(:load_synthdefs)
      internal_load_synthdef(path, server)
    end

    def sample_loaded?(path)
      return true if path.is_a?(Buffer)
      path = File.expand_path(path)
      return @samples.has_key?(path)
    end

    def load_sample(path, server=@server)
      check_for_server_rebooting!(:load_sample)
      internal_load_sample(path, server)
    end

    def free_sample(paths, server=@server)
      check_for_server_rebooting!(:free_sample)
      @sample_sem.synchronize do
        paths.each do |p|
          p = File.expand_path(p)
          info = @samples[p]
          @samples.delete(p)
          server.buffer_free(info) if info
        end
      end
      :free
    end

    def free_all_samples(server=@server)
      check_for_server_rebooting!(:free_all_samples)
      @sample_sem.synchronize do
        @samples.each do |k, v|
          server.buffer_free(v)
        end
        @samples = {}
      end
    end


    def start_amp_monitor
      check_for_server_rebooting!(:start_amp_monitor)
      unless @amp_synth
        @amp_synth = @server.trigger_synth :head, @monitor_group, "sonic-pi-amp_stereo_monitor", {"bus" => 0}, true
      end
    end

    def kill_live_synth(name_id)
      check_for_server_rebooting!(:kill_live_synth)
      @server.kill_live_synth(name_id)
    end

    def trigger_live_synth(name_id, synth_name, group, args, info, now=false, t_minus_delta=false, pos=:tail, pre_trig, on_move_blk)
      check_for_server_rebooting!(:trigger_live_synth)
      @server.trigger_live_synth(name_id, pos, group, synth_name, args, info, now, t_minus_delta, pre_trig, on_move_blk)
    end

    # Ensure a Link Audio subscription is active for (peer, channel) and
    # return its audio bus index. Each tuple gets its own bus pair,
    # allocated lazily and kept across :stop / re-trigger so a user FX
    # chain pointing at it keeps working.
    def ensure_link_audio_input(peer, channel, link_api)
      check_for_server_rebooting!(:ensure_link_audio_input)
      key = [peer, channel]
      @link_audio_mut.synchronize do
        bus = @link_audio_subs[key] ||= @server.allocate_audio_bus
        # Idempotent on (peer, channel); re-issuing keeps the receive
        # buffer alive. Stream is rendered stereo into (bus, bus+1).
        link_api.link_audio_input_set!(peer, channel, bus.to_i)
        bus.to_i
      end
    end

    # Stop one (peer, channel) Link Audio stream, or every stream for the
    # peer when channel is nil.
    def kill_link_audio(peer, channel, link_api)
      check_for_server_rebooting!(:kill_link_audio)
      @link_audio_mut.synchronize do
        keys = if channel
                 @link_audio_subs.key?([peer, channel]) ? [[peer, channel]] : []
               else
                 @link_audio_subs.keys.select { |k| k.first == peer }
               end
        # Kill the live synth(s); each on_destroyed fires
        # link_audio_input_gone, which drops the SuperSonic subscription.
        # Bus records stay so a re-trigger reuses the bus pair.
        keys.each { |k| @server.kill_live_synth(k) }
      end
    end

    # Called from a link_audio synth's on_destroyed; drops just that
    # SuperSonic subscription. Bus record stays for a re-trigger.
    def link_audio_input_gone(peer, channel, link_api)
      link_api.link_audio_input_remove!(peer, channel) if link_api
    end

    # Drop every SuperSonic Link Audio subscription at once. Cold-swap only:
    # the World rebuild fires no node callbacks, so the per-synth
    # on_destroyed teardown never runs; engine subs survive but point at
    # stale busses.
    def kill_all_link_audio(link_api)
      return unless link_api
      @link_audio_mut.synchronize do
        link_api.link_audio_inputs_clear! unless @link_audio_subs.empty?
      end
    end

    def trigger_synth(synth_name, group, args, info, now=false, t_minus_delta=false, pos=:tail )
      check_for_server_rebooting!(:trigger_synth)

      # A nil group means an aborted cold-swap left the studio without its
      # base groups (mixer_group is nil until the retry pass completes).
      # Raise something a musician can act on rather than nil.subnode_add.
      if group.nil?
        raise StudioCurrentlyRebootingError,
              "The audio engine is still reinitialising after a device change - please try again in a moment"
      end

      @server.trigger_synth(pos, group, synth_name, args, info, now, t_minus_delta)
    end

    def set_volume(vol, now=false, silent=false)
      check_for_server_rebooting!(:invert)
      @volume = vol
      message "Setting main volume to #{vol}" unless silent
      @server.node_ctl @mixer, {"pre_amp" => vol * 0.2}, now
    end

    def mixer_invert_stereo(invert)
      check_for_server_rebooting!(:mixer_invert_stereo)
      @mixer_invert_stereo = invert
      invert_i = invert ? 1 : 0
      @server.node_ctl @mixer, {"invert_stereo" => invert_i}, true
    end

    def mixer_control(opts)
      check_for_server_rebooting!(:mixer_control)
      now = 0
      opts = opts.clone
      if opts[:now].is_a?(Numeric)
        now = opts[:now]
      else
        now = opts[:now] ? 1 : 0
      end
      opts.delete :now
      @server.node_ctl @mixer, opts, now
    end

    def mixer_reset
      check_for_server_rebooting!(:mixer_reset)
      info = Synths::SynthInfo.get_info(:main_mixer)
      mixer_control(info.slide_arg_defaults)
      mixer_control(info.arg_defaults)
    end

    def mixer_stereo_mode
      check_for_server_rebooting!(:mixer_stereo_mode)
      @mixer_force_mono = false
      @server.node_ctl @mixer, {"force_mono" => 0}, true
    end

    def mixer_mono_mode
      check_for_server_rebooting!(:mixer_mono_mode)
      @mixer_force_mono = true
      @server.node_ctl @mixer, {"force_mono" => 1}, true
    end

    def status
      check_for_server_rebooting!(:status)
      @server.status
    end

    def stop
      check_for_server_rebooting!(:stop)
      @server.clear_schedule
      @server.group_clear @synth_group
    end

    def new_group(position, target, name="")
      check_for_server_rebooting!(:new_group)
      @server.create_group(position, target, name)
    end

    def new_synth_group(id=-1)
      check_for_server_rebooting!(:new_synth_group)
      new_group(:tail, @synth_group, "Run-#{id}-Synths")
    end

    def new_fx_group(id=-1)
      check_for_server_rebooting!(:new_fx_group)
      new_group(:tail, @fx_group, "Run-#{id}-FX")
    end

    def new_fx_bus
      check_for_server_rebooting!(:new_fx_bus)
      @server.allocate_audio_bus
    end

    def control_delta
      @server.control_delta
    end

    def control_delta=(t)
      @server.control_delta = t
    end

    def recording?
      ! @recorders.empty?
    end

    def bit_depth=(depth)
      @sample_format = case depth
                       when 8
                         "int8"
                       when 16
                         "int16"
                       when 24
                         "int24"
                       when 32
                         "int32"
                       else
                         raise "Unknown recording bit depth: #{depth}.\nExpected one of 8, 16, 24 or 32."
                       end
    end

    def recording_start(path, bus=0)
      check_for_server_rebooting!(:recording_start)
      return false if @recorders[bus]
      @recording_mutex.synchronize do
        return false if @recorders[bus]
        # Use SuperSonic's JUCE-side recording (taps the main audio
        # output before it leaves the engine, written via JUCE's
        # TimeSliceThread). The previous scsynth-internal recorder
        # used the `sonic-pi-recorder` synthdef which depends on the
        # DiskOut UGen — that isn't ported into SuperSonic, so the
        # synthdef fails to load and the synth never starts.
        #
        # The bus argument is preserved for API compatibility but
        # ignored by the JUCE tap, which always records bus 0 (the
        # main output mix). Non-zero bus recording was rarely used
        # and is no worse than the previous scsynth-internal path,
        # which was also broken without DiskOut.
        if bus != 0
          message "recording: bus=#{bus} ignored — only main output (bus 0) is recorded"
        end
        @server.osc "/supersonic/record/start", path, "wav", 24
        @recorders[bus] = [path]
        true
      end
    end

    def save_buffer!(buf, path)
      @server.buffer_write(buf, path, "wav", @sample_format)
    end

    def recording_stop(bus=0)
      check_for_server_rebooting!(:recording_stop)
      return false unless @recorders[bus]
      @recording_mutex.synchronize do
        return false unless @recorders[bus]
        @server.osc "/supersonic/record/stop"
        @recorders.delete bus

        # ensure nodes are all paused if we are in a paused state
        @server.node_pause(0, true) if @paused

        true
      end
    end

    def shutdown
      @server_reboot.kill
      begin
        @server.shutdown
      rescue Exception
      end
    end

    # Nuke scsynth state on cold swap — all node/bus/buffer refs are stale.
    def nuke_scsynth_state!
      log_message "Nuking studio scsynth state"
      @recording_mutex.synchronize do
        # JUCE-side recording writes to a temp path; on cold swap the
        # supersonic engine itself stops the recording as part of its
        # device teardown, so we just drop the bookkeeping. (Previously
        # this freed scsynth-side buffer-stream handles.)
        @recorders = {}
      end
      @buffers = {}
      @samples = {}
      @control_busses = {}
      @amp_synth = nil
      @mixer = nil
      @scope = nil
      @synth_group = nil
      @fx_group = nil
      @mixer_group = nil
      @monitor_group = nil
      @mixer_bus = nil
      log_message "Studio scsynth state nuked"
    end

    # Rebuild everything after a cold swap. Force-replaces @reboot_mutex
    # after 15s if a previous reinit is stuck — safe only because the
    # mutex is private to this method.
    def cold_swap_reinit!
      start = Time.now
      acquired = false
      deadline = Time.now + 15
      while Time.now < deadline
        if @reboot_mutex.try_lock
          acquired = true
          break
        end
        sleep 0.1
      end

      unless acquired
        STDOUT.puts "WARNING: previous reinit stuck, forcing new mutex"
        STDOUT.flush
        @reboot_mutex = Mutex.new
        @reboot_mutex.lock
      end

      # Acquire the WRITER lock for the entire reinit. Blocks until
      # all in-flight studio methods (trigger_synth, new_group, etc.)
      # finish — guarantees they don't see partially-nilled studio
      # state. The gate is reentrant on this thread so the phase code
      # below (which calls studio methods like start_mixer) can still
      # acquire the read lock without deadlocking.
      @studio_ready_gate.with_studio_writer do
      begin
        @cold_swap_reinit_in_progress = true
        @cold_swap_reinit_thread = Thread.current
        @rebooting = true
        message "Reinitialising after device change..."

        # Phase-failure logging with backtrace — `message` alone loses context
        log_phase_err = lambda do |label, e|
          STDOUT.puts "[ruby-error] Studio #{label}: #{e.class}: #{e.message}"
          (e.backtrace || []).first(15).each { |f| STDOUT.puts "[ruby-error]   #{f}" }
          STDOUT.flush
          message "Error #{label}: #{e.message}"
        end

        begin
          @server.nuke_scsynth_state!
          nuke_scsynth_state!
          STDOUT.puts "Studio - Phase 1: Nuke (#{(Time.now - start).round(2)}s)"
          STDOUT.flush
        rescue Exception => e
          log_phase_err.call("nuking state", e)
        end

        # Rebuild needs Studio methods to work — open the gate
        @rebooting = false

        # Phase 1.5: Re-register Spider as a /supersonic/notify target.
        # supersonic builds a fresh World on driver-switch / cold-swap, and
        # the new World's notify-subscribers list is empty. If we skip this,
        # Phase 2's /d_loadDir and Phase 3's /sync (in clear_scsynth!) send
        # fine but the /synced + /done replies are silently dropped — both
        # promises hit their 10s/5s timeouts and mixer_group stays nil.
        # Since every reinit pass (including the debouncer's retry passes)
        # would fail the same way, skipping this leaves studio broken until
        # the next device event or a relaunch. This is what blocked ASIO
        # from producing sound after a driver switch.
        begin
          ok = @server.register_for_notifications!(timeout: 5.0)
          STDOUT.puts "Studio - Phase 1.5: Notify re-register #{ok ? 'OK' : 'TIMEOUT'} (#{(Time.now - start).round(2)}s)"
          STDOUT.flush
        rescue Exception => e
          log_phase_err.call("re-registering notify target", e)
        end

        # Phase 2: Load synthdefs into the new World. Must precede
        # Phase 3 — the server-info query there runs the
        # sonic-pi-server-info synthdef.
        begin
          @server.load_synthdefs(Paths.synthdef_path)
          STDOUT.puts "Studio - Phase 2: Synthdefs (#{(Time.now - start).round(2)}s)"
          STDOUT.flush
        rescue Exception => e
          log_phase_err.call("loading synthdefs", e)
        end

        # Phase 3: Re-read server info, then rebuild groups and busses.
        # The new World's hardware channel counts set the audio bus
        # allocator's reserved offset; skipping the refresh leaves fx
        # busses overlapping hardware output/input busses when the
        # device channel count changed (mic feedback, synths bypassing
        # the mixer).
        begin
          @server.fetch_scsynth_info!(5)
          reset_and_setup_groups_and_busses
          STDOUT.puts "Studio - Phase 3: Server info + Groups (#{(Time.now - start).round(2)}s)"
          STDOUT.flush
        rescue Exception => e
          log_phase_err.call("resetting groups", e)
        end

        # Phases 4-6 all need the mixer group, which Phase 3's
        # reset_and_setup_groups_and_busses creates. It's nil when a reply
        # was lost mid-swap (a second /supersonic/setup wiping the /notify
        # subscribers list, or Phase 3's fetch_scsynth_info! /sync timing
        # out), so running them anyway just produces noisy `nil.subnode_add`
        # NoMethodErrors. Skip cleanly: __cold_swap_reinit! reports the pass
        # as incomplete and the debounce thread in spider-server.rb schedules
        # its own retry pass — it must not wait for another /supersonic/setup,
        # which never comes when the timeout was the swap's last event. Runs
        # arriving in the window get StudioCurrentlyRebootingError from
        # trigger_synth's nil-group guard.
        if @mixer_group.nil?
          STDOUT.puts "Studio - reinit pass incomplete (mixer group nil) — " \
                      "skipping mixer/scope/init; a retry will be scheduled"
          STDOUT.flush
          message "Reinitialisation incomplete (retrying shortly...)"
        else
          # Phase 4: Start mixer and reapply GUI settings (firing from
          # updateAudioDeviceConfig targets the dead pre-swap node)
          begin
            start_mixer
            set_volume(@volume, true, true) if @volume
            mixer_invert_stereo(@mixer_invert_stereo) if @mixer_invert_stereo
            if @mixer_force_mono
              mixer_mono_mode
            end
            STDOUT.puts "Studio - Phase 4: Mixer (#{(Time.now - start).round(2)}s)"
            STDOUT.flush
          rescue Exception => e
            log_phase_err.call("starting mixer", e)
          end

          # Phase 5: Start scope
          begin
            start_scope
            STDOUT.puts "Studio - Phase 5: Scope (#{(Time.now - start).round(2)}s)"
            STDOUT.flush
          rescue Exception => e
            log_phase_err.call("starting scope", e)
          end

          # Phase 6: Init studio (synthdefs, samples, rand buffer)
          begin
            init_studio
            STDOUT.puts "Studio - Phase 6: Init (#{(Time.now - start).round(2)}s)"
            STDOUT.flush
          rescue Exception => e
            log_phase_err.call("in init_studio", e)
          end

          message "Reinitialisation complete (#{(Time.now - start).round(2)}s)"
        end
      ensure
        @rebooting = false
        @cold_swap_reinit_in_progress = false
        @cold_swap_reinit_thread = nil
        # Stamp completion time BEFORE releasing the writer lock so
        # the first reader that's been waiting at the gate sees the
        # fresh timestamp on its next sleep timing-check and gets the
        # grace window. (Trigger thread wakes up → does its work →
        # next sleep call checks last_cold_swap_completed_at — must
        # be already set.)
        @last_cold_swap_completed_at = Time.now.to_f
        @reboot_mutex.unlock if @reboot_mutex.owned?
        # Wake any threads parked on `wait_for_reboot_complete`.
        @reboot_done_mutex.synchronize { @reboot_done_cv.broadcast }
      end
      end  # with_studio_writer — end of writer-locked block
    end

    # Block the calling thread until any in-flight cold-swap reinit
    # finishes (or the timeout elapses). Returns true if the studio is
    # ready (or no reinit is in progress), false if the timeout fired
    # first.
    #
    # Same-thread bypass: cold_swap_reinit's own phases call back into
    # Studio methods (e.g. start_mixer → trigger_synth). Those calls
    # must NOT wait or they'd deadlock. Detected via Thread.current ==
    # @cold_swap_reinit_thread and short-circuited.
    def wait_for_reboot_complete(timeout=20)
      return true if Thread.current == @cold_swap_reinit_thread
      return true unless @cold_swap_reinit_in_progress
      @reboot_done_mutex.synchronize do
        deadline = Time.now + timeout
        while @cold_swap_reinit_in_progress
          remaining = deadline - Time.now
          return false if remaining <= 0
          @reboot_done_cv.wait(@reboot_done_mutex, remaining)
        end
      end
      true
    end

    def pause(silent=true)
      @recording_mutex.synchronize do
        unless recording? || @paused
          @server.node_pause(0, true)
          message "Pausing SuperSonic Audio Server" unless silent
        end
        @paused = true
      end
    end

    def start(silent=true)
      @recording_mutex.synchronize do
        if @paused
          @server.node_run(0, true)
          message "Resuming SuperSonic Audio Server" unless silent
        end
        @paused = false
      end
    end

    def control_bus(name)
      check_for_server_rebooting!(:control_bus)
      return @control_busses[name] if @control_busses.has_key?(name)

      @control_bus_mutex.synchronize do
        bus = @server.allocate_control_bus
        @control_busses[name] = bus
        return bus
      end
    end

    def control_bus_set(name, val)
      bus = control_bus(name)
      if bus
        @server.control_bus_set(bus, val)
      end
      return bus
    end

    def set_audio_latency!(latency)
      @server.set_latency!(latency)
    end

    def set_global_timewarp!(time)
      @server.set_global_timewarp!(time)
    end

    # Block until studio is ready, then yield. Replaces the old
    # check-then-raise gate (`check_for_server_rebooting!`) — the old
    # one raised mid-trigger if a cold-swap fired AFTER the check
    # passed but BEFORE the trigger finished, killing live loops with
    # a backtrace and producing nil node references that crashed
    # FXNode#initialize. Now the gate is held for the duration of the
    # caller's work, so cold_swap_reinit can't start until all in-
    # flight triggers finish, and it blocks new triggers from
    # starting until the swap is done. Reentrant per-thread.
    def with_studio_ready(op_name=nil, &block)
      @studio_ready_gate.with_studio_ready(op_name || :anonymous, &block)
    end

    private

    # Legacy shim: every studio method that used to call this now
    # wraps its body in `with_studio_ready` instead. Kept as a no-op
    # so any straggling call sites don't break, but the real work is
    # done by the gate.
    def check_for_server_rebooting!(msg=nil)
      # The new gate handles this — see with_studio_ready / @studio_ready_gate.
      # Intentionally a no-op now; the wrapper that calls this method
      # is the one that holds the read lock.
    end

    def log_message(s)
      s = "Studio - #{s}"
      Kernel.puts s
      log s
    end

    def message(s)
      m = s.to_s
      @msg_queue.push({:type => :info, :val => m}) unless __system_thread_locals.get :sonic_pi_spider_silent
      log_message(m)
    end


    def reset_and_setup_groups_and_busses
      log_message "Reset and setup groups and busses"
      log_message "Clearing scsynth"
      # AudioBus allocator is about to be wiped; drop subscription records
      # so a post-reset link_audio call allocates fresh.
      @link_audio_mut.synchronize { @link_audio_subs.clear }
      @server.reset!
      log_message "Allocating audio bus"
      @mixer_bus = @server.allocate_audio_bus

      log_message "Create Base Synth Groups"
      @mixer_group = @server.create_group(:head, 0, "STUDIO-MIXER")
      @fx_group = @server.create_group(:before, @mixer_group, "STUDIO-FX")
      @synth_group = @server.create_group(:before, @fx_group, "STUDIO-SYNTHS")
      @monitor_group = @server.create_group(:after, @mixer_group, "STUDIO-MONITOR")
    end

    def reset_server
      log_message "Resetting server"
      reset_and_setup_groups_and_busses
      start_mixer
      start_scope
    end

    def start_mixer
      # TODO create a way of swapping these on the fly:
      # set_mixer! :basic
      # set_mixer! :default
      log_message "Starting mixer"
      mixer_synth = "sonic-pi-mixer"
      # Pre-apply user's pre_amp — otherwise amp=6 * default pre_amp=1.0
      # bursts at full blast for ~100ms before set_volume kicks in
      initial_pre_amp = @volume ? @volume * 0.2 : 0.2
      @mixer = @server.trigger_synth(:head, @mixer_group, mixer_synth,
                                      {"in_bus" => @mixer_bus.to_i,
                                       "amp" => 6,
                                       "pre_amp" => initial_pre_amp},
                                      nil, true)
    end

    def start_scope
      log_message "Starting scope"
      scope_synth = "sonic-pi-scope"
      @scope = @server.trigger_synth(:head, @monitor_group, scope_synth, { "max_frames" => 1024 })
    end


    def internal_load_sample(path, server=@server)
      path = File.expand_path(path)
      return [@samples[path], true] if @samples[path]
      #message "Loading full sample path: #{path}"
      sample_info = nil
      @sample_sem.synchronize do
        return @samples[path] if @samples[path]
        raise "No sample exists with path:\n  #{unify_tilde_dir(path).inspect}" unless File.exist?(path) && !File.directory?(path)
        buf_info = server.buffer_alloc_read(path)
        sample_info = SampleBuffer.new(buf_info, path)
        @samples[path] = sample_info
      end

      [sample_info, false]
    end

    def internal_load_synthdefs(path, server=@server)
      return internal_load_synthdef if File.file?(path)
      @sample_sem.synchronize do
        server.load_synthdefs(path)
        @loaded_synthdefs << path
      end
    end

    def internal_load_synthdef(path, server=@server)
      @sample_sem.synchronize do
        server.load_synthdef(path)
        @loaded_synthdefs << path
      end
    end

    # ── Studio gate wiring ───────────────────────────────────────────────
    #
    # Every public studio method that touches scsynth (used to call
    # check_for_server_rebooting!(:foo) at the top) is now wrapped to
    # acquire the read lock for its full duration. cold_swap_reinit
    # holds the write lock around its phases and waits for in-flight
    # readers to drain before Phase 1's nuke runs.
    #
    # We use Module#prepend rather than rewriting each method body —
    # the wrapper is uniform (5 lines) and the list of gated methods
    # lives in one place where it's easy to audit. The prepended
    # `super` call invokes the original method body, which still
    # contains a `check_for_server_rebooting!(:foo)` call — that call
    # is a NO-OP now (kept as a shim) and the real gating happens here.
    #
    # cold_swap_reinit holds the write lock; reentrant gate means its
    # OWN calls into these methods (start_mixer, etc.) succeed.
    GATED_STUDIO_METHODS = %i[
      allocate_buffer free_buffer
      load_synthdefs load_synthdef
      load_sample free_sample free_all_samples
      start_amp_monitor
      kill_live_synth trigger_live_synth trigger_synth
      set_volume mixer_invert_stereo mixer_control mixer_reset
      mixer_stereo_mode mixer_mono_mode
      status stop
      new_group new_synth_group new_fx_group new_fx_bus
      recording_start recording_stop
      control_bus
    ].freeze

    _gate_module = Module.new
    GATED_STUDIO_METHODS.each do |m|
      _gate_module.module_eval do
        define_method(m) do |*args, **kwargs, &blk|
          @studio_ready_gate.with_studio_ready(m) do
            super(*args, **kwargs, &blk)
          end
        end
      end
    end
    prepend(_gate_module)

  end
end
