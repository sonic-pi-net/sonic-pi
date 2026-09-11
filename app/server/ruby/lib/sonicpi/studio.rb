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

    attr_reader :synth_group, :fx_group, :mixer_group, :monitor_group, :mixer_id, :mixer_bus, :mixer, :rand_buf_id, :amp, :rebooting, :last_cold_swap_completed_at, :server

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
      # The engine's tracks, by name, as last broadcast on /clockwork/track/list.
      # A track is studio state made in the GUI; code only names it, so this
      # is the whole of what the language needs: which harness channels a
      # name sends to and returns on. @track_lane_base is 0 until the engine
      # has lanes for tracks at all.
      @tracks = {}
      @track_lane_base = 0
      @tracks_mut = Mutex.new
      @tracks_cv = ConditionVariable.new
      @tracks_generation = 0
      # Each plugin's parameter names, by handle, as the engine pages them
      # out on /clockwork/track/plugin/params. Filled on demand: the first
      # track_control on a track asks for its plugins' lists, so an unknown
      # name can be refused with the names that would have worked instead of
      # doing nothing in silence on the audio thread.
      @track_params = {}
      @track_param_cache = {}
      # A TRACK IS HEARD WITHOUT BEING ASKED. Every track the engine lists
      # has a monitor here, by id: a live_audio_stereo on the track's return
      # into the mixer, so `track_midi :e3, track: :surge` sounds with nothing else
      # written, and a plugin's own keyboard sounds from its window with no
      # code running at all. That is a DAW's rule - a track's output reaches
      # the master until it is routed somewhere else - and `live_track` is
      # the routing: it takes the monitor's place while it runs
      # (track_monitor_claim). The stream is then the live_track's: when
      # Stop kills it, the track's audio goes with it - Stop means silence,
      # not a switch to the main mix - and the track is heard again when
      # something next plays or sends into it (track_monitor_park /
      # track_touched). Only `live_track :name, :stop`, code asking for the
      # stream to go while code still runs, hands the track straight back
      # (track_monitor_release). @track_claimed holds the ids a live_track
      # has or had; @track_parked the ones whose live_track has gone.
      @track_monitors = {}
      @track_claimed = Set.new
      @track_parked = Set.new
      @track_monitor_mut = Mutex.new
      # Made here, not in init_studio: the first track list can arrive, and
      # the monitors with it, before init_studio has run.
      @recorders = {}
      @recording_mutex = Mutex.new
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

    # The :piano synth's sample table reaches the plugin the way a sample
    # reaches the engine: as a buffer, which /supersonic/piano/wavetable points
    # the plugin at. The asset ships as raw 16-bit integers and the engine
    # reads audio files, so it is given a WAV header once, in the user's
    # Sonic Pi directory (the app's own tree may be read-only).
    def load_piano_wavetable
      dat = Paths.piano_wavetable_path
      unless File.exist?(dat)
        message "Piano wavetable not found at #{dat} — :piano will be silent"
        return
      end
      cache = File.join(Paths.home_dir_path, "cache")
      FileUtils.mkdir_p(cache)
      wav = File.join(cache, "piano_wavetable.wav")
      Studio.write_piano_wavetable_wav(dat, wav)
      buf = @server.buffer_alloc_read(wav)
      buf.wait_for_allocation
      @server.piano_wavetable(buf)
      @server.buffer_free(buf)
    end

    # The raw table as a mono 16-bit WAV, rewritten only when the asset is
    # newer than the last copy. The rate is nominal: the plugin indexes the
    # table itself and never asks.
    def self.write_piano_wavetable_wav(dat, wav)
      return wav if File.exist?(wav) && File.mtime(wav) >= File.mtime(dat)
      data = File.binread(dat)
      rate = 44100
      header = ["RIFF", 36 + data.bytesize, "WAVE",
                "fmt ", 16, 1, 1, rate, rate * 2, 2, 16,
                "data", data.bytesize].pack("a4Va4a4VvvVVvva4V")
      File.binwrite(wav, header + data)
      wav
    end

    def init_studio
      @server.load_synthdefs(Paths.synthdef_path)
      load_piano_wavetable
      @amp = [0.0, 1.0]
      @server.add_event_handler("/sonic-pi/amp", "/sonic-pi/amp") do |payload|
        @amp = [payload[2], payload[3]]
      end

      # The engine broadcasts the track list on every edit, and answers a
      # request with the same payload under .reply; both keep the registry.
      ["/clockwork/track/list", "/clockwork/track/list.reply"].each do |addr|
        @server.add_event_handler(addr, addr) do |payload|
          __receive_track_list(payload)
        end
      end
      @server.add_event_handler("/clockwork/track/state", "/clockwork/track/state") do |payload|
        id, gain, mute = payload[0], payload[1], payload[2]
        @tracks_mut.synchronize do
          t = @tracks.values.find { |x| x[:id] == id }
          if t
            t[:gain] = gain.to_f
            t[:mute] = mute.to_i == 1
          end
        end
      end
      @server.add_event_handler("/clockwork/track/plugin/params", "/clockwork/track/plugin/params") do |payload|
        __receive_track_params(payload)
      end
      request_track_list

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

    # ── Tracks ──────────────────────────────────────────────────────────────

    def request_track_list
      @server.osc "/clockwork/track/list"
    rescue Exception => e
      STDOUT.puts "Studio - track list request failed: #{e.message}"
    end

    # /clockwork/track/list <lane_base> <count> then per track
    #   <id> <slot> <name> <send_ch> <return_ch> <gain> <mute> <node_count>
    #   then per node <handle> <is_instrument> <bypass> <channel> <name>
    #   <vendor> <format> <path> <index> <latency>
    # The field counts here ARE the wire format: one field out and every
    # track after the first with a plugin reads as garbage (a track once
    # showed up as :1 - its id, where its name should have been).
    # Channels are the engine's 0-based; the language wants 1-based bus
    # numbers, added on the way out (track_send_channel).
    def __receive_track_list(payload)
      p = payload.to_a
      lane_base = p[0].to_i
      count     = p[1].to_i
      i = 2
      tracks = {}
      count.times do
        id, slot, name, send_ch, return_ch, gain, mute, node_count = p[i, 8]
        i += 8
        nodes = []
        node_count.to_i.times do
          handle, is_inst, bypass, channel, nname, vendor, format, path, index, latency = p[i, 10]
          i += 10
          nodes << {handle: handle.to_i, instrument: is_inst.to_i == 1, bypass: bypass.to_i == 1,
                    channel: channel.to_i,
                    name: nname.to_s, vendor: vendor.to_s, format: format.to_s,
                    path: path.to_s, index: index.to_i, latency: latency.to_i}
        end
        tracks[name.to_s] = {id: id.to_i, slot: slot.to_i, name: name.to_s,
                             send: send_ch.to_i, return: return_ch.to_i,
                             gain: gain.to_f, mute: mute.to_i == 1, nodes: nodes}
      end
      @tracks_mut.synchronize do
        @tracks = tracks
        @track_lane_base = lane_base
        @track_param_cache.clear
        @tracks_generation += 1
        @tracks_cv.broadcast
      end
      __sync_track_monitors
    rescue Exception => e
      STDOUT.puts "Studio - malformed track list: #{e.message}"
    end

    # The level a run's own mixer gives everything it plays (job_mixer in
    # lang/sound.rb), so a track sounds the same by default as it does
    # through live_track.
    TRACK_MONITOR_AMP = 0.3

    # Bring the monitors into line with the track list: one for every
    # track no live_track has, none for a track that has gone. Reads the
    # registry rather than taking a list, so it serves the list's arrival,
    # a live_track's release and a rebuilt server alike.
    def __sync_track_monitors
      return if @rebooting || @mixer_group.nil?
      wanted = @tracks_mut.synchronize do
        @tracks.values.map { |t| [t[:id], t[:return]] }.to_h
      end
      @track_monitor_mut.synchronize do
        @track_claimed &= wanted.keys.to_set
        @track_parked &= @track_claimed
        @track_monitors.each do |id, m|
          # Gone, or moved to another return: its monitor reads the wrong lane.
          next if wanted[id] == m[:return]
          m[:node].kill(true)
          @track_monitors.delete(id)
        end
        wanted.each do |id, ret|
          next if @track_monitors.key?(id) || @track_claimed.include?(id)
          info = Synths::SynthInfo.get_info(:live_audio_stereo)
          node = @server.trigger_synth(:head, @mixer_group, "sonic-pi-live_audio_stereo",
                                       {"input" => ret + 1, "out_bus" => @mixer_bus.to_i,
                                        "amp" => TRACK_MONITOR_AMP}, info, true)
          @track_monitors[id] = {node: node, return: ret}
        end
      end
      # A track made while nothing ran must be heard now, not on the next
      # run: the graph was paused for want of anything to play.
      @recording_mutex.synchronize do
        @server.node_run(0, true) if @paused && tracks?
      end
    rescue Exception => e
      STDOUT.puts "Studio - track monitor sync failed: #{e.message}"
    end

    def tracks?
      @tracks_mut.synchronize { !@tracks.empty? }
    end

    # A live_track is taking the track's audio into a run. The monitor goes
    # at spider time, when the live synth starts, so the track neither
    # doubles nor drops out at the handover.
    def track_monitor_claim(id)
      @track_monitor_mut.synchronize do
        @track_claimed << id
        m = @track_monitors.delete(id)
        m[:node].kill(false) if m
      end
    end

    # The live_track's node has gone - Stop, or a kill. The track stays
    # silent: the stream was the live_track's.
    def track_monitor_park(id)
      @track_monitor_mut.synchronize do
        @track_parked << id if @track_claimed.include?(id)
      end
    end

    # `live_track :name, :stop`: the track is heard on the main mix again.
    # Order-proof against the node's own park: a released id is not
    # claimed, so a park arriving later does nothing.
    def track_monitor_release(id)
      @track_monitor_mut.synchronize do
        @track_claimed.delete(id)
        @track_parked.delete(id)
      end
      __sync_track_monitors
    end

    # Something is playing or sending into the track. A parked one is heard
    # on the main mix again; any other is left as it is.
    def track_touched(name)
      t = track_info(name)
      return unless t
      id = t[:id]
      parked = @track_monitor_mut.synchronize do
        @track_parked.delete?(id) && @track_claimed.delete(id)
      end
      __sync_track_monitors if parked
    end

    def track_id(name)
      track_lookup!(name)[:id]
    end

    def track_names
      @tracks_mut.synchronize { @tracks.keys.dup }
    end

    def track_info(name)
      @tracks_mut.synchronize { t = @tracks[name.to_s]; t && t.dup }
    end

    # The track's picture, or a fresh one from the engine. A name the
    # registry doesn't know is first asked about — a broadcast may have
    # been missed, or the track made a moment ago — and only then refused,
    # with the names that would have worked.
    def track_lookup!(name)
      key = name.to_s
      t = track_info(key)
      return t if t
      gen = @tracks_mut.synchronize { @tracks_generation }
      request_track_list
      @tracks_mut.synchronize do
        deadline = Time.now + 1.0
        while @tracks_generation == gen && (rem = deadline - Time.now) > 0
          @tracks_cv.wait(@tracks_mut, rem)
        end
        t = @tracks[key]
        return t.dup if t
        if @track_lane_base == 0
          raise "This SuperSonic has no track lanes, so :#{key} cannot be reached from code. Tracks need a build with plugin hosting."
        end
        known = @tracks.keys
        hint = known.empty? ? "There are no tracks yet - make one in the Tracks panel." :
                              "Tracks: #{known.map { |k| ":#{k}" }.join(", ")}"
        raise "Unknown track :#{key}. #{hint}"
      end
    end

    # /clockwork/track/plugin/params <handle> <total> <offset> <count>
    #   per param: <id> <name> <min> <max> <value> <group> <group_name> <automatable>
    # One page. Only the names are kept; the values belong to the GUI.
    #
    # THE PAGES ARE BROADCAST, AND THE GUI HEARS THEM TOO. Every page is
    # taken in, wherever it came from - a list the panel fetched is a list
    # track_control need not - but the NEXT page is asked for only when this
    # one is new, and only for a list this side asked for. Two listeners
    # that each continued every page they heard made two requests per page,
    # four per page after that, and Surge's sixty pages became seventeen
    # thousand.
    def __receive_track_params(payload)
      p = payload.to_a
      handle, total, offset, count = p[0].to_i, p[1].to_i, p[2].to_i, p[3].to_i
      names = count.times.map { |k| p[4 + k * 8 + 1].to_s }
      # min and max, by name: what a value is checked against before it is
      # sent. A plugin's range is whatever its own controller says — a JUCE
      # plugin such as Surge XT says 0..1 for every knob.
      ranges = {}
      count.times { |k| ranges[names[k]] = [p[4 + k * 8 + 2].to_f, p[4 + k * 8 + 3].to_f] }
      advance = false
      @tracks_mut.synchronize do
        list = (@track_params[handle] ||= {total: total, names: [], ranges: {}, got: 0, wanted: false})
        list[:total] = total
        if offset == 0 && list[:got] > 0
          list[:names] = []
          list[:ranges] = {}
          list[:got] = 0
        end
        if offset == list[:got]
          list[:names].concat(names)
          list[:ranges].merge!(ranges)
          list[:got] = offset + count
          advance = list[:wanted] && count > 0 && list[:got] < total
        end
        @track_param_cache.clear
        @tracks_generation += 1
        @tracks_cv.broadcast
      end
      __request_track_params(handle, offset + count) if advance
    rescue Exception => e
      STDOUT.puts "Studio - malformed track parameter page: #{e.message}"
    end

    def __request_track_params(handle, offset = 0)
      @tracks_mut.synchronize do
        list = (@track_params[handle] ||= {total: 0, names: [], ranges: {}, got: 0, wanted: false})
        list[:wanted] = true
      end
      @server.osc "/clockwork/track/plugin/params", handle.to_i, offset.to_i
    rescue Exception => e
      STDOUT.puts "Studio - track parameter request failed: #{e.message}"
    end

    # True once every plugin on the track has its whole parameter list.
    def __track_params_complete?(t)
      t[:nodes].all? do |n|
        l = @track_params[n[:handle]]
        l && l[:got] >= l[:total]
      end
    end

    # The opt key a plugin parameter goes by in code: "Filter 1 Cutoff" is
    # filter_1_cutoff:. Lower case, every run of anything but a letter or
    # digit made one underscore, none at the ends; a leading digit gets an
    # underscore in front so it can be a symbol. The editor's completion
    # applies the same rule (app/gui/utils/trackparam.h); they must agree.
    def self.track_param_key(name)
      k = name.to_s.downcase.gsub(/[^a-z0-9]+/, "_").gsub(/\A_+|_+\z/, "")
      k = "_" + k if k =~ /\A\d/
      k
    end

    # A parameter the track's plugins know by that name — exactly, ignoring
    # case (the engine's own rule), or by its opt key — resolved to the name
    # the plugin uses, from the first plugin in the chain that has it, with
    # that plugin's handle and the parameter's min and max.
    # Refused, with the nearest names, when none does: a knob that does not
    # exist is the commonest way for a control to do nothing. Looked up on
    # every note that carries a parameter, so the answers are kept; the
    # cache goes whenever the tracks or a list change.
    def track_param_lookup!(name, param)
      t = track_lookup!(name)
      key = param.to_s
      raise "Track :#{t[:name]} has no plugins yet, so there is no parameter to set. Add one in the Tracks panel." if t[:nodes].empty?
      cached = @tracks_mut.synchronize { @track_param_cache[[t[:name], key]] }
      return cached if cached
      complete = @tracks_mut.synchronize { __track_params_complete?(t) }
      unless complete
        t[:nodes].each do |n|
          l = @tracks_mut.synchronize { @track_params[n[:handle]] }
          __request_track_params(n[:handle], l ? l[:got] : 0) if l.nil? || l[:got] < l[:total]
        end
        @tracks_mut.synchronize do
          deadline = Time.now + 2.0
          while !__track_params_complete?(t) && (rem = deadline - Time.now) > 0
            @tracks_cv.wait(@tracks_mut, rem)
          end
        end
      end
      # Each plugin's names, in chain order, with the plugin they belong to.
      # The answer is a name AND a handle: the engine sets that plugin's
      # parameter, not the first in the chain that happens to share the name.
      per_node = @tracks_mut.synchronize do
        t[:nodes].map { |n| [n, (@track_params[n[:handle]] || {names: []})[:names]] }
      end
      with_range = lambda do |f, node|
        r = @tracks_mut.synchronize { (@track_params[node[:handle]] || {})[:ranges].to_h[f] }
        [f, node[:handle], *r]
      end
      k = Studio.track_param_key(key)
      resolve = lambda do |names|
        names.find { |n| n == key } || names.find { |n| n.casecmp?(key) } ||
          (k.empty? ? nil : names.find { |n| Studio.track_param_key(n) == k })
      end
      found = nil
      per_node.each do |node, names|
        f = resolve.call(names)
        found = with_range.call(f, node) if f
        break if found
      end
      # A key under the plugin's own name — surge_xt_effects_mix: — reaches a
      # plugin further down the chain than the first to have a Mix.
      unless found || k.empty?
        per_node.each do |node, names|
          pk = Studio.track_param_key(node[:name])
          next if pk.empty? || !k.start_with?("#{pk}_")
          rest = k[(pk.length + 1)..-1]
          f = names.find { |n| Studio.track_param_key(n) == rest }
          found = with_range.call(f, node) if f
          break if found
        end
      end
      if found
        @tracks_mut.synchronize { @track_param_cache[[t[:name], key]] = found }
        return found
      end
      # Unknown. The plugin's names are a better answer than "no".
      names = per_node.flat_map { |_, ns| ns }
      words = key.downcase.split(/[\s_]+/).reject(&:empty?)
      near = names.select { |n| d = n.downcase; words.any? { |w| d.include?(w) } }.uniq.first(8)
      hint = near.empty? ? "The parameter names are on the device in the Tracks panel, or in its own window." :
                           "Did you mean: #{near.map { |n| "#{Studio.track_param_key(n)}: (#{n.inspect})" }.join(", ")}"
      raise "Unknown parameter #{key.inspect} on track :#{t[:name]}. #{hint}"
    end

    # 1-based, as the sound_out_stereo FX and live_audio synths count.
    def track_send_channel(name)
      track_lookup!(name)[:send] + 1
    end

    def track_return_channel(name)
      track_lookup!(name)[:return] + 1
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

    # Drive: how hard the mix is pushed into the limiter. Sets the mixer's
    # pre_amp, which sits before the limiter, so raising it makes the mix
    # louder and denser rather than simply louder, and its meter is gain
    # reduction, not level.
    #
    # `vol` is a plain linear gain: 1.0 is unity, and it reaches pre_amp
    # unscaled so that one number describes what the control does.
    def set_drive(vol, now=false, silent=false)
      check_for_server_rebooting!(:set_drive)
      @drive = vol
      message "Setting drive to #{vol}" unless silent
      @server.node_ctl @mixer, {"pre_amp" => vol}, now
      notify_mixer_settings
    end

    # Both default to unity until explicitly set: the mixer is started with
    # pre_amp and amp at 1 when no value has been pushed yet.
    def drive
      @drive || 1.0
    end

    def volume
      @volume || 1.0
    end

    # Volume: the fader after the limiter, clamped to unity. Unlike drive
    # it cannot change the sound or push anything into the ceiling: only
    # how loud the result comes out. The synthdef clamps too; this clamp is
    # so the stored value and the audible one agree.
    def set_volume(vol, now=false, silent=false)
      check_for_server_rebooting!(:set_volume)
      vol = 0.0 if vol < 0.0
      vol = 1.0 if vol > 1.0
      @volume = vol
      message "Setting volume to #{vol}" unless silent
      @server.node_ctl @mixer, {"amp" => vol}, now
      notify_mixer_settings
    end

    # Tell observers (the GUI's Volume and Drive dials) the mixer's levels,
    # whoever changed them: dials and code share this state, and a dial
    # showing a stale value silently reverts the newer one next time it is
    # touched. Sent even for silent changes, which suppress only the log.
    def notify_mixer_settings
      @msg_queue.push({:type => :mixer_settings,
                       :drive => drive,
                       :output_volume => volume})
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
        # The front (SuperSonic's process) records the master tap and
        # answers record/start.reply <ok> <path|error>; wait for it so a
        # refusal is reported here rather than discovered at save time.
        t0 = Time.now
        reply = @server.osc_with_reply("/clockwork/record/start.reply", 2, "/clockwork/record/start", path, "wav", 24)
        log_message "recording: start -> #{reply.inspect} (#{((Time.now - t0) * 1000).round} ms)"
        if reply.nil?
          message "recording: no reply from the engine to record/start"
        elsif reply[0].to_i != 1
          message "recording: could not start — #{reply[1]}"
          return false
        end
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
        # Wait for record/stop.reply: the file is finished only once the
        # front has closed it, and a save moves it straight after this.
        t0 = Time.now
        reply = @server.osc_with_reply("/clockwork/record/stop.reply", 5, "/clockwork/record/stop")
        log_message "recording: stop -> #{reply.inspect} (#{((Time.now - t0) * 1000).round} ms)"
        message "recording: no reply from the engine to record/stop" if reply.nil?
        @recorders.delete bus

        # ensure nodes are all paused if we are in a paused state
        __pause_graph if @paused

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

        # Phase 1.5: Re-register Spider as a /clockwork/notify target.
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
          # The lanes may sit at a new base after a device change; the
          # list carries it.
          request_track_list
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
        # was lost mid-swap (a second /clockwork/setup wiping the /notify
        # subscribers list, or Phase 3's fetch_scsynth_info! /sync timing
        # out), so running them anyway just produces noisy `nil.subnode_add`
        # NoMethodErrors. Skip cleanly: __cold_swap_reinit! reports the pass
        # as incomplete and the debounce thread in spider-server.rb schedules
        # its own retry pass — it must not wait for another /clockwork/setup,
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
            set_drive(@drive, true, true) if @drive
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
          __pause_graph
          message "Pausing SuperSonic Audio Server" unless silent
        end
        @paused = true
      end
    end

    # Nothing is running: rest the graph. Unless a track is on it - a track
    # is heard whether or not code runs (its plugin's own keyboard, a
    # reverb's tail after Stop), so with tracks present the graph stays up
    # and what stops is the NOTES. Every instrument gets its notes off,
    # which is what Stop means on a DAW, and what a note needs when the
    # thread holding its note-off has just been killed.
    def __pause_graph
      @server.track_all_notes_off(nil) if tracks?
      @server.node_pause(0, true) unless tracks?
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
      # Every live_track is gone with the server, and none is coming back
      # to claim: every track is heard on the main mix from the restart.
      @track_monitor_mut.synchronize do
        @track_monitors.clear
        @track_claimed.clear
        @track_parked.clear
      end
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
      __sync_track_monitors
    end

    def start_mixer
      # TODO create a way of swapping these on the fly:
      # set_mixer! :basic
      # set_mixer! :default
      log_message "Starting mixer"
      mixer_synth = "sonic-pi-mixer"
      # Pre-apply the user's drive rather than starting at the synthdef
      # default and correcting a moment later, otherwise the mix bursts at
      # full blast for ~100ms before set_drive lands.
      initial_pre_amp = @drive || 1.0
      @mixer = @server.trigger_synth(:head, @mixer_group, mixer_synth,
                                      {"in_bus" => @mixer_bus.to_i,
                                       "amp" => @volume || 1.0,
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
      set_volume set_drive mixer_invert_stereo mixer_control mixer_reset
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
